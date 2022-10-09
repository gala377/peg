pub trait Property<Typ, Get, Set> {
    fn get(&self, d: &Typ) -> Get;
    fn set(&self, d: &mut Typ, v: Set);
}

impl<Typ, Get, Set, GetFn, SetFn> Property<Typ, Get, Set> for (GetFn, SetFn)
where
    GetFn: Fn(&Typ) -> Get,
    SetFn: Fn(&mut Typ, Set),
{
    fn get(&self, d: &Typ) -> Get {
        (self.0)(d)
    }

    fn set(&self, d: &mut Typ, v: Set) {
        (self.1)(d, v)
    }
}
