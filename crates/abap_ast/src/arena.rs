//! Flat/arena syntax tree: one [`Vec`] of node headers and one contiguous child-id buffer.
//!
//! This layout matches patterns used in many production compilers and front ends:
//! - **rustc** places many IR nodes in typed arenas (`Arena<T>`) so traversals hit contiguous memory.
//! - **Swift AST**, **Clang** modules, and several **WebAssembly** toolchains use similar
//!   “node id + side table of edges” designs for cache locality and a single allocation bucket.
//! - **rust-analyzer** builds on **rowan** ([`GreenNode`](https://docs.rs/rowan) / red-green trees)
//!   for immutable structural sharing during incremental reparsing—different trade-off (Rc,
//!   hashing) than this module, which optimises for parse-once / analyse-once memory footprint.
//!
//! [`crate::SyntaxTreeBuilder`] is what the workspace parser uses. [`crate::SyntaxNode`] remains
//! for tests and for [`SyntaxTree::from_nested`].

use abap_lexer::TextRange;

use crate::SyntaxKind;

/// Opaque index into a [`SyntaxTree`]'s `nodes` vector.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct NodeId(pub u32);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ArenaNode {
    kind: SyntaxKind,
    start: u32,
    end: u32,
    first_child: u32,
    child_count: u32,
}

/// Immutable syntax tree in arena layout.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTree {
    nodes: Vec<ArenaNode>,
    child_indices: Vec<u32>,
    root: NodeId,
}

impl SyntaxTree {
    pub fn root(&self) -> NodeId {
        self.root
    }

    #[inline]
    pub fn kind(&self, id: NodeId) -> SyntaxKind {
        self.nodes[id.0 as usize].kind
    }

    #[inline]
    pub fn range(&self, id: NodeId) -> TextRange {
        let n = self.nodes[id.0 as usize];
        n.start as usize..n.end as usize
    }

    pub fn children(&self, id: NodeId) -> impl DoubleEndedIterator<Item = NodeId> + Clone + '_ {
        let n = self.nodes[id.0 as usize];
        let s = n.first_child as usize;
        let e = s + n.child_count as usize;
        self.child_indices[s..e].iter().copied().map(NodeId)
    }

    /// First node in preorder under `start` (including `start`) with `kind`.
    pub fn find_first_kind(&self, start: NodeId, kind: SyntaxKind) -> Option<NodeId> {
        let mut stack = vec![start];
        while let Some(id) = stack.pop() {
            if self.kind(id) == kind {
                return Some(id);
            }
            for c in self.children(id).rev() {
                stack.push(c);
            }
        }
        None
    }

    /// First direct child of `parent` with `kind`.
    pub fn child_by_kind(&self, parent: NodeId, kind: SyntaxKind) -> Option<NodeId> {
        self.children(parent).find(|&c| self.kind(c) == kind)
    }

    /// Depth-first count of nodes with `kind` (for tests and benchmarks).
    pub fn count_kind(&self, id: NodeId, kind: SyntaxKind) -> usize {
        let mut stack = vec![id];
        let mut n = 0usize;
        while let Some(cur) = stack.pop() {
            let node = self.nodes[cur.0 as usize];
            n += usize::from(node.kind == kind);
            let s = node.first_child as usize;
            let e = s + node.child_count as usize;
            for idx in (s..e).rev() {
                stack.push(NodeId(self.child_indices[idx]));
            }
        }
        n
    }

    /// Converts a recursive [`crate::SyntaxNode`] tree into arena layout (one large allocation
    /// for headers, one for child ids).
    pub fn from_nested(root: &crate::SyntaxNode) -> Self {
        let mut b = SyntaxTreeBuilder::default();
        let rid = b.push_node(root);
        b.finish(rid)
    }
}

/// Builds a [`SyntaxTree`] bottom-up.
#[derive(Default)]
pub struct SyntaxTreeBuilder {
    nodes: Vec<ArenaNode>,
    child_indices: Vec<u32>,
}

impl SyntaxTreeBuilder {
    #[inline]
    pub fn span(&self, id: NodeId) -> TextRange {
        let n = &self.nodes[id.0 as usize];
        n.start as usize..n.end as usize
    }

    pub fn leaf(&mut self, kind: SyntaxKind, range: TextRange) -> NodeId {
        assert!(range.start <= u32::MAX as usize && range.end <= u32::MAX as usize);
        let id = self.nodes.len() as u32;
        self.nodes.push(ArenaNode {
            kind,
            start: range.start as u32,
            end: range.end as u32,
            first_child: 0,
            child_count: 0,
        });
        NodeId(id)
    }

    pub fn branch(&mut self, kind: SyntaxKind, range: TextRange, children: &[NodeId]) -> NodeId {
        assert!(range.start <= u32::MAX as usize && range.end <= u32::MAX as usize);
        let first_child = self.child_indices.len() as u32;
        for c in children {
            self.child_indices.push(c.0);
        }
        let id = self.nodes.len() as u32;
        self.nodes.push(ArenaNode {
            kind,
            start: range.start as u32,
            end: range.end as u32,
            first_child,
            child_count: children.len() as u32,
        });
        NodeId(id)
    }

    pub fn finish(self, root: NodeId) -> SyntaxTree {
        SyntaxTree {
            nodes: self.nodes,
            child_indices: self.child_indices,
            root,
        }
    }

    fn push_node(&mut self, node: &crate::SyntaxNode) -> NodeId {
        if node.children.is_empty() {
            return self.leaf(node.kind, node.range.clone());
        }
        let mut ids = Vec::with_capacity(node.children.len());
        for c in &node.children {
            ids.push(self.push_node(c));
        }
        self.branch(node.kind, node.range.clone(), &ids)
    }
}

/// Estimated retained memory for the nested representation (nodes + `Vec` control words only).
pub fn nested_retained_bytes(node: &crate::SyntaxNode) -> usize {
    let self_bytes = std::mem::size_of::<crate::SyntaxNode>();
    let vec_cap = node.children.capacity() * std::mem::size_of::<crate::SyntaxNode>();
    let children_sum: usize = node.children.iter().map(nested_retained_bytes).sum();
    self_bytes + vec_cap + children_sum
}

/// Retained memory for [`SyntaxTree`] (continuous buffers).
pub fn arena_retained_bytes(tree: &SyntaxTree) -> usize {
    tree.nodes.capacity() * std::mem::size_of::<ArenaNode>()
        + tree.child_indices.capacity() * std::mem::size_of::<u32>()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::SyntaxNode;

    fn count_nested(node: &SyntaxNode, kind: SyntaxKind) -> usize {
        let mut n = usize::from(node.kind == kind);
        for c in &node.children {
            n += count_nested(c, kind);
        }
        n
    }

    #[test]
    fn from_nested_matches_recursive_counts() {
        let leaf = |k| SyntaxNode::leaf(k, 0..1);
        let tree = SyntaxNode::branch(
            SyntaxKind::File,
            0..10,
            vec![
                leaf(SyntaxKind::Token),
                SyntaxNode::branch(
                    SyntaxKind::BinaryExpr,
                    1..9,
                    vec![
                        leaf(SyntaxKind::ExprIdent),
                        leaf(SyntaxKind::Token),
                        leaf(SyntaxKind::ExprLiteral),
                    ],
                ),
            ],
        );
        let flat = SyntaxTree::from_nested(&tree);
        for k in [
            SyntaxKind::File,
            SyntaxKind::BinaryExpr,
            SyntaxKind::Token,
            SyntaxKind::ExprIdent,
            SyntaxKind::ExprLiteral,
        ] {
            assert_eq!(
                flat.count_kind(flat.root(), k),
                count_nested(&tree, k),
                "kind {:?}",
                k
            );
        }
    }
}
