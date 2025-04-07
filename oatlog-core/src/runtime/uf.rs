use crate::runtime::Eclass;

// TODO: consider multi-subscriber queue
// - for `new` when adding rule scheduling
// - for `uprooted` maybe already

/// The main union-find.
/// Per eclass state.
#[derive(Default)]
pub struct UnionFind<T> {
    repr: Vec<u32>,
    /// Newly created eclasses since last `take_new`
    new: Vec<T>,
    /// Uprooted eclasses as of before starting the latest canonicalization round.
    uprooted_snapshot: Vec<T>,
    /// Uprooted eclasses since last `uprooted` snapshotting.
    uprooted: Vec<T>,
}
impl<T> std::fmt::Debug for UnionFind<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f)?;
        for i in 0..self.repr.len() {
            let repr = self.repr[i];
            if repr == i as u32 {
                writeln!(f, "{i}")?;
            } else {
                writeln!(f, "{i}: {repr}")?;
            }
        }
        Ok(())
    }
}
impl<T: Eclass> UnionFind<T> {
    #[inline]
    #[must_use]
    pub fn new() -> Self {
        Self {
            repr: Vec::new(),
            new: Vec::new(),
            uprooted_snapshot: Vec::new(),
            uprooted: Vec::new(),
        }
    }
    /// Sugar
    #[inline]
    pub fn are_equal(&mut self, a: T, b: T) -> bool {
        self.find(a) == self.find(b)
    }
    /// Sugar
    #[inline]
    pub fn union_mut(&mut self, a: &mut T, b: &mut T) {
        let ret = self.union(*a, *b);
        *a = ret;
        *b = ret;
    }
    /// Sugar
    #[inline]
    pub fn find(&mut self, t: T) -> T {
        T::new(self.find_inner(t.inner()))
    }
    /// Sugar
    #[inline]
    pub fn is_root(&mut self, t: T) -> bool {
        self.find(t) == t
    }
    #[inline]
    pub fn already_canonical(&mut self, t: &mut T) -> bool {
        let i = t.inner();
        if self.repr[i as usize] == i {
            true
        } else {
            let root = self.find_inner(self.repr[i as usize]);
            self.repr[i as usize] = root;
            *t = T::new(root);
            false
        }
    }
    #[inline]
    fn find_inner(&mut self, i: u32) -> u32 {
        if self.repr[i as usize] == i {
            i
        } else {
            let root = self.find_inner(self.repr[i as usize]);
            self.repr[i as usize] = root;
            root
        }
    }
    #[inline]
    pub fn union(&mut self, a: T, b: T) -> T {
        let a = self.find_inner(a.inner());
        let b = self.find_inner(b.inner());
        if a == b {
            return T::new(a);
        }
        let (root, uprooted) = (u32::min(a, b), u32::max(a, b));
        self.repr[uprooted as usize] = self.repr[root as usize];
        self.uprooted.push(T::new(uprooted));
        T::new(root)
    }
    #[inline]
    pub fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        self.new.push(T::new(id));
        T::new(id)
    }
    /// Called within rule matching. Since query planning constructs at most one `forall x` of a
    /// given type, clearing `new` here is correct.
    #[inline]
    pub fn take_new(&mut self) -> impl Iterator<Item = T> {
        debug_assert!(self.new.is_sorted());
        let ret = Vec::clone(&self.new).into_iter();
        self.new.clear();
        ret
    }
    #[inline]
    pub fn create_uprooted_snapshot(&mut self) {
        self.uprooted_snapshot.clear();
        self.uprooted_snapshot.extend_from_slice(&self.uprooted);
        self.uprooted_snapshot.sort_unstable();
        self.uprooted.clear();
    }
    #[inline]
    pub fn get_uprooted_snapshot(&self) -> &[T] {
        &self.uprooted_snapshot
    }
    #[inline]
    pub fn has_new_uproots(&self) -> bool {
        !self.uprooted.is_empty()
    }
}
