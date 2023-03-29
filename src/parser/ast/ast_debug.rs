//! Code used to debug an AST.

/// The trait used for debugging an AST.
pub trait ASTDebug {

    /// The function to use when printing an AST
    fn print(&self) {
        self.print_impl(0);
    }

    /// Used internally by [print], should not be called explicitly.
    fn print_impl(&self, depth: u32) {
        let mut next_depth = depth;
        if let Some(name) = self.name() {
            for _ in 0..depth * 2 {
                print!(" ");
            }

            println!("{}", name);
            next_depth += 1;
        }
        for child in self.chidren().iter() {
            child.print_impl(next_depth);
        }
    }

    /// Needs to be implemented to implement [ASTDebug]. Defines the list
    /// of children nodes.
    fn chidren(&self) -> Vec<&dyn ASTDebug>;

    /// Needs to be implemented to implement [ASTDebug]. Defines the name
    /// of the AST node. If [None] the node will not be printed.
    fn name(&self) -> Option<String> {
        None
    }
}
