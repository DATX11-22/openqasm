pub trait ASTDebug {
    fn print(&self) {
        self.print_impl(0);
    }

    fn print_impl(&self, depth: u32) {
        for _ in 0..depth * 4 {
            print!(" ");
        }

        println!("{}", self.name());
        for child in self.chidren().iter() {
            child.print_impl(depth + 1);
        }
    }

    fn chidren(&self) -> Vec<&dyn ASTDebug>;
    fn name(&self) -> String;
}


