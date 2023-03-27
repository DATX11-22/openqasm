pub trait ASTDebug {
    fn print(&self) {
        self.print_impl(0);
    }

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

    fn chidren(&self) -> Vec<&dyn ASTDebug>;
    fn name(&self) -> Option<String> {
        None
    }
}


