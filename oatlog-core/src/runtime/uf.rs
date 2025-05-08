use crate::runtime::Eclass;

// TODO: consider multi-subscriber queue
// - for `new` when adding rule scheduling
// - for `uprooted` maybe already

/// The main union-find.
/// Per eclass state.
#[derive(Default)]
pub struct UnionFind<T> {
    /// `repr[i] == 0` for roots.
    /// Parent of `i` is `i-repr[i]` otherwise.
    repr: Vec<u32>,
    /// Newly created eclasses since last `take_new`
    new: Vec<T>,
    num_uprooted: usize,
    num_roots: usize,
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
            num_uprooted: 0,
            num_roots: 0,
        }
    }
    #[inline]
    #[must_use]
    pub fn len(&self) -> usize {
        self.repr.len()
    }
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }
    #[inline]
    #[must_use]
    pub fn num_roots(&self) -> usize {
        self.num_roots
    }
    /// Sugar
    #[inline]
    pub fn are_equal(&mut self, a: T, b: T) -> bool {
        self.find(a) == self.find(b)
    }
    /// Sugar
    #[inline]
    pub fn union_mut(&mut self, a: &mut T, b: &mut T) -> T {
        let ret = self.union(*a, *b);
        *a = ret;
        *b = ret;
        ret
    }
    /// Sugar
    #[inline]
    pub fn find(&mut self, t: T) -> T {
        T::new(self.find_inner(t.inner()))
    }
    /// Sugar
    #[inline]
    #[allow(unsafe_code)]
    pub fn is_root(&mut self, t: T) -> bool {
        // NOTE: Using a bitset is not beneficial

        // SAFETY: The only way to construct `T` is using `add_eclass` below, which also push to `repr`.
        unsafe {
            debug_assert!((t.inner() as usize) < self.repr.len());
            *self.repr.get_unchecked(t.inner() as usize) == t.inner()
        }
    }
    #[inline]
    #[allow(unsafe_code)]
    fn find_inner(&mut self, mut i: u32) -> u32 {
        // Curiously, it is not beneficial to
        // 1. Unroll this loop
        // 2. Unroll this loop with path compression
        // 3. Perform recursive path compression (well how could it given (2))

        // TODO erik for loke: if this assert is needed for performance or something please
        // document that.
        assert!((i as usize) < self.repr.len());
        loop {
            let i_old = i;
            // SAFETY: `i` being in bounds initially is checked above, and UF parents will always
            // be in bounds.
            unsafe {
                debug_assert!((i as usize) < self.repr.len());
                i = *self.repr.get_unchecked(i as usize);
            }
            if i == i_old {
                break i;
            }
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
        self.repr[uprooted as usize] = root;
        self.num_uprooted += 1;
        self.num_roots -= 1;
        T::new(root)
    }
    #[inline]
    pub fn add_eclass(&mut self) -> T {
        let id = u32::try_from(self.repr.len()).expect("out of u32 ids");
        self.repr.push(id);
        self.new.push(T::new(id));
        self.num_roots += 1;
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
    #[must_use]
    pub fn num_uprooted(&self) -> usize {
        self.num_uprooted
    }
    #[inline]
    pub fn reset_num_uprooted(&mut self) {
        self.num_uprooted = 0;
    }
}
