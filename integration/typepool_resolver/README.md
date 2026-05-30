# Type-Pool Resolver Endpoint

This folder contains the optional SAP-side ICF handler used by the analyzer after normal ADT dependency lookup misses type-pool symbols.

## Deploy

1. Create class `ZCL_ABAPLS_TYPEPOOL_RESOLVER` from `zcl_abapls_typepool_resolver.abap`.
2. Create an ICF service in `SICF`, for example `/sap/bc/zabapls/typepool`.
3. Set the service handler list to `ZCL_ABAPLS_TYPEPOOL_RESOLVER`.
4. Activate the class and ICF service.
5. Configure the analyzer environment:

```text
ABAP_TYPEPOOL_RESOLVER_URL=https://your.sap.host/sap/bc/zabapls/typepool
```

The analyzer reuses the existing ADT username, password, SAP client, cookies, and HTTP timeout.

## Contract

Owner lookup:

```text
GET /sap/bc/zabapls/typepool?op=owner&name=TPAK_PERMISSION_TO_USE_LIST
200 text/plain
TPAK
```

Source lookup:

```text
GET /sap/bc/zabapls/typepool?op=source&pool=TPAK
200 text/plain
TYPE-POOL tpak.
...
```

Non-2xx responses are treated as misses.

## Notes

The handler avoids ABAP source parsing. For owner lookup it tries every prefix ending before an underscore, then searches `DDTYPET-TYPEGROUP` with descending leading prefixes up to five characters. Candidate pools are verified through `DDTYPET` or, as a fallback, readable source.

Type-pool source is read from the generated program name `%_C<typegroup>`, for example `%_CABAP` for type group `ABAP`. If a system stores type-pool source under a different generated report name, adjust `read_typepool_source`.
