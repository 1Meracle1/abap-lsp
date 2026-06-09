#Requires -Version 5.1
<#
.SYNOPSIS
Clears Zed's cached ABAP tree-sitter grammar when the cached clone is stale or dirty.

.DESCRIPTION
Zed installs this dev extension as a junction and keeps grammar build output under
grammars/<grammar>. If that cached clone has local changes, Zed cannot checkout the
rev from extension.toml and keeps using the old WASM parser. This script removes
only the grammar cache directory and WASM file so Zed can rebuild them.

.EXAMPLE
.\reset-grammar-cache.ps1

Clears a stale or dirty ABAP grammar cache and asks you to reload/rebuild the dev extension.

.EXAMPLE
.\reset-grammar-cache.ps1 -RestartZed

Closes Zed, clears the cache, restarts Zed, and waits for the WASM parser to reappear.
#>
[CmdletBinding(SupportsShouldProcess = $true)]
param(
  [string]$ExtensionId = "abap",
  [string]$Grammar = "abap",
  [string]$ZedRoot = $(if ($env:LOCALAPPDATA) { Join-Path $env:LOCALAPPDATA "Zed" } else { "" }),
  [switch]$RestartZed,
  [int]$RestartWaitSeconds = 15,
  [int]$RebuildWaitSeconds = 60,
  [switch]$Force
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

function Get-FullPath {
  param([Parameter(Mandatory = $true)][string]$Path)

  return [System.IO.Path]::GetFullPath($Path).TrimEnd('\', '/')
}

function Assert-ChildPath {
  param(
    [Parameter(Mandatory = $true)][string]$Path,
    [Parameter(Mandatory = $true)][string]$Parent,
    [Parameter(Mandatory = $true)][string]$Description
  )

  $fullPath = Get-FullPath $Path
  $fullParent = Get-FullPath $Parent
  $comparison = [StringComparison]::OrdinalIgnoreCase

  if (
    -not $fullPath.Equals($fullParent, $comparison) -and
    -not $fullPath.StartsWith($fullParent + [System.IO.Path]::DirectorySeparatorChar, $comparison)
  ) {
    throw "$Description path '$fullPath' is outside expected root '$fullParent'."
  }
}

function Get-GrammarRevFromManifest {
  param(
    [Parameter(Mandatory = $true)][string]$ManifestPath,
    [Parameter(Mandatory = $true)][string]$GrammarName
  )

  $sectionPattern = "^\s*\[grammars\.$([regex]::Escape($GrammarName))\]\s*$"
  $inSection = $false

  foreach ($line in Get-Content -LiteralPath $ManifestPath) {
    if ($line -match "^\s*\[.*\]\s*$") {
      $inSection = $line -match $sectionPattern
      continue
    }

    if ($inSection -and $line -match '^\s*rev\s*=\s*"([^"]+)"\s*$') {
      return $Matches[1]
    }
  }

  throw "Could not find rev in [grammars.$GrammarName] in '$ManifestPath'."
}

function Invoke-Git {
  param(
    [Parameter(Mandatory = $true)][string]$Repository,
    [Parameter(Mandatory = $true)][string[]]$Arguments
  )

  if (-not (Get-Command git -ErrorAction SilentlyContinue)) {
    return $null
  }

  $output = & git -C $Repository @Arguments 2>$null
  if ($LASTEXITCODE -ne 0) {
    return $null
  }

  return ($output -join "`n").Trim()
}

if (-not $ZedRoot) {
  throw "LOCALAPPDATA is not set. Pass -ZedRoot explicitly."
}

$extensionRoot = Get-FullPath $PSScriptRoot
$manifestPath = Join-Path $extensionRoot "extension.toml"
if (-not (Test-Path -LiteralPath $manifestPath)) {
  throw "Expected Zed extension manifest at '$manifestPath'."
}

$manifestRev = Get-GrammarRevFromManifest -ManifestPath $manifestPath -GrammarName $Grammar
$installedExtension = Join-Path (Join-Path (Join-Path $ZedRoot "extensions") "installed") $ExtensionId
$cacheRoot = Join-Path $installedExtension "grammars"

if (-not (Test-Path -LiteralPath $installedExtension)) {
  Write-Warning "Zed dev extension '$ExtensionId' is not installed at '$installedExtension'. Falling back to extension-local cache."
  $cacheRoot = Join-Path $extensionRoot "grammars"
}

$cacheRootFull = Get-FullPath $cacheRoot
$repoCache = Join-Path $cacheRootFull $Grammar
$wasmCache = Join-Path $cacheRootFull "$Grammar.wasm"

Assert-ChildPath -Path $repoCache -Parent $cacheRootFull -Description "Grammar repository cache"
Assert-ChildPath -Path $wasmCache -Parent $cacheRootFull -Description "Grammar WASM cache"

$cachedRev = $null
$dirty = $false
if (Test-Path -LiteralPath (Join-Path $repoCache ".git")) {
  $cachedRev = Invoke-Git -Repository $repoCache -Arguments @("rev-parse", "HEAD")
  $status = Invoke-Git -Repository $repoCache -Arguments @("status", "--porcelain")
  $dirty = -not [string]::IsNullOrWhiteSpace($status)
}

$wasmExists = Test-Path -LiteralPath $wasmCache
$stale = $cachedRev -and ($cachedRev -ne $manifestRev)
$missingRepo = -not (Test-Path -LiteralPath $repoCache)
$shouldReset = $Force -or $stale -or $dirty -or $missingRepo -or -not $wasmExists

Write-Host "Manifest rev: $manifestRev"
if ($cachedRev) {
  Write-Host "Cached rev:   $cachedRev"
} else {
  Write-Host "Cached rev:   <none>"
}
Write-Host "Cache root:   $cacheRootFull"

if (-not $shouldReset) {
  Write-Host "Zed grammar cache already matches the manifest and is clean."
  exit 0
}

$zedProcesses = @(Get-Process -Name Zed -ErrorAction SilentlyContinue)
$zedLaunchPath = ($zedProcesses |
  Where-Object { $_.Path } |
  Select-Object -ExpandProperty Path -First 1)

if ($RestartZed -and $zedProcesses.Count -gt 0) {
  $processIds = ($zedProcesses | ForEach-Object { $_.Id }) -join ", "
  Write-Host "Closing Zed before resetting grammar cache (PID $processIds)."

  if ($PSCmdlet.ShouldProcess("Zed process(es) $processIds", "Close before resetting grammar cache")) {
    foreach ($process in $zedProcesses) {
      if ($process.MainWindowHandle -ne 0) {
        [void]$process.CloseMainWindow()
      }
    }

    $deadline = (Get-Date).AddSeconds($RestartWaitSeconds)
    while ((Get-Date) -lt $deadline) {
      $remaining = @(Get-Process -Name Zed -ErrorAction SilentlyContinue)
      if ($remaining.Count -eq 0) {
        break
      }
      Start-Sleep -Milliseconds 250
    }

    $remaining = @(Get-Process -Name Zed -ErrorAction SilentlyContinue)
    if ($remaining.Count -gt 0) {
      $remainingIds = ($remaining | ForEach-Object { $_.Id }) -join ", "
      throw "Zed did not close within $RestartWaitSeconds seconds (PID $remainingIds). Close it manually, then rerun this script."
    }
  }
} elseif ($zedProcesses.Count -gt 0) {
  $processIds = ($zedProcesses | ForEach-Object { $_.Id }) -join ", "
  Write-Warning "Zed is currently running (PID $processIds). Close/reopen Zed or run the in-app dev-extension rebuild after this reset."
}

if ($stale) {
  Write-Host "Reason: cached grammar revision is stale."
}
if ($dirty) {
  Write-Host "Reason: cached grammar repository has local changes."
}
if ($missingRepo) {
  Write-Host "Reason: cached grammar repository is missing."
}
if (-not $wasmExists) {
  Write-Host "Reason: cached WASM parser is missing."
}
if ($Force) {
  Write-Host "Reason: -Force was supplied."
}

$targets = @($repoCache, $wasmCache)
foreach ($target in $targets) {
  if (-not (Test-Path -LiteralPath $target)) {
    continue
  }

  Assert-ChildPath -Path $target -Parent $cacheRootFull -Description "Cache target"
  if ($PSCmdlet.ShouldProcess($target, "Remove stale Zed grammar cache")) {
    Remove-Item -LiteralPath $target -Recurse -Force
    Write-Host "Removed $target"
  }
}

if ($RestartZed) {
  if (-not $zedLaunchPath) {
    $zedCommand = Get-Command zed -ErrorAction SilentlyContinue
    if ($zedCommand) {
      $zedLaunchPath = $zedCommand.Source
    }
  }

  if ($zedLaunchPath) {
    if (-not $PSCmdlet.ShouldProcess($zedLaunchPath, "Start Zed")) {
      exit 0
    }

    Start-Process -FilePath $zedLaunchPath
    Write-Host "Started Zed. Waiting up to $RebuildWaitSeconds seconds for $Grammar.wasm."

    $deadline = (Get-Date).AddSeconds($RebuildWaitSeconds)
    while ((Get-Date) -lt $deadline) {
      if (Test-Path -LiteralPath $wasmCache) {
        Write-Host "Detected rebuilt parser at $wasmCache."
        exit 0
      }
      Start-Sleep -Seconds 1
    }

    Write-Warning "Zed was restarted, but $Grammar.wasm was not recreated within $RebuildWaitSeconds seconds. Check Zed logs or run the in-app dev-extension rebuild."
    exit 1
  }

  Write-Warning "Could not find a Zed launcher. Start Zed manually and rebuild the ABAP dev extension."
  exit 1
}

Write-Host "Reload or rebuild the ABAP dev extension in Zed. Zed should now checkout $manifestRev and rebuild $Grammar.wasm."
