#![feature(box_syntax, box_patterns)]

#[allow(unused)]
pub mod grammar {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Val {
    Unit,
    Left(Box<Val>),
    Right(Box<Val>),
    Pair(Box<Val>, Box<Val>),
    Neg(Box<Val>),
    Frac(Box<Val>),
    Symbol(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Iso {
    // Zeroe, Zeroi,
    SwapS,
    AssoclS, AssocrS,
    Unite, Uniti,
    SwapP,
    AssoclP, AssocrP,
    Distrib, Factor,
    EtaS, EpsilonS,
    EtaP, EpsilonP,
}

impl Iso {
    pub fn step(self, val: Val) -> Val {
        use Iso::*;
        use Val::*;

        match (self, val) {
            (SwapS, Left(v)) => Right(v),
            (SwapS, Right(v)) => Left(v),
            (AssoclS, Left(v1)) => Left(box Left(v1)),
            (AssoclS, Right(box Left(v2))) => Left(box Right(v2)),
            (AssoclS, Right(box Right(v3))) => Right(v3),
            (AssocrS, Left(box Left(v1))) => Left(v1),
            (AssocrS, Left(box Right(v2))) => Right(box Left(v2)),
            (AssocrS, Right(v3)) => Right(box Right(v3)),
            (Unite, Pair(box Unit, v)) => *v,
            (Uniti, v) => Pair(box Unit, box v),
            (SwapP, Pair(v1, v2)) => Pair(v2, v1),
            (AssoclP, Pair(v1, box Pair(v2, v3))) => Pair(box Pair(v1, v2), v3),
            (AssocrP, Pair(box Pair(v1, v2), v3)) => Pair(v1, box Pair(v2, v3)),
            (Distrib, Pair(box Left(v1), v3)) => Left(box Pair(v1, v3)),
            (Distrib, Pair(box Right(v2), v3)) => Left(box Pair(v2, v3)),
            (Factor, Left(box Pair(v1, v3))) => Pair(box Left(v1), v3),
            (Factor, Right(box Pair(v2, v3))) => Pair(box Right(v2), v3),
            _ => unimplemented!()
        }
    }
    pub fn dual(self) -> Self {
        use Iso::*;

        match self {
            SwapS => SwapS,
            AssoclS => AssocrS,
            AssocrS => AssoclS,
            Unite => Uniti,
            Uniti => Unite,
            SwapP => SwapP,
            AssoclP => AssocrP,
            AssocrP => AssoclP,
            Distrib => Factor,
            Factor => Distrib,
            EtaS => EpsilonS,
            EpsilonS => EtaS,
            EtaP => EpsilonP,
            EpsilonP => EtaP,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Comb {
    Iso(Iso),
    Id,
    Sym(Box<Comb>),
    Seq(Box<Comb>, Box<Comb>),
    Sum(Box<Comb>, Box<Comb>),
    Prod(Box<Comb>, Box<Comb>),
}

impl Comb {
    pub fn dual(self) -> Self {
        use Comb::*;

        match self {
            Iso(iso) => Iso(iso.dual()),
            Id => Id,
            Sym(c) => Sym(box c.dual()),
            Seq(c1, c2) => Seq(box c2.dual(), box c1.dual()),
            Sum(c1, c2) => Sum(box c1.dual(), box c2.dual()),
            Prod(c1, c2) => Prod(box c1.dual(), box c2.dual()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum CombCtx {
    Empty,
    Fst(Box<CombCtx>, Comb),
    Snd(Comb, Box<CombCtx>),
    L_P(Box<CombCtx>, Comb, Val),
    R_P(Comb, Val, Box<CombCtx>),
    L_S(Box<CombCtx>, Comb),
    R_S(Comb, Box<CombCtx>),
}

#[derive(Debug, Clone)]
pub enum MachineState {
    Pre(Comb, Val, CombCtx),
    Post(Comb, Val, CombCtx),
}

impl MachineState {
    pub fn step(self) -> Self {
        use MachineState::*;
        use CombCtx::*;
        use Comb::*;

        match self {
            Pre(Iso(iso), val, ctx) => Post(Iso(iso), iso.step(val), ctx),
            Pre(Id, val, ctx) => Post(Id, val, ctx),
            Pre(Sym(c), val, ctx) => Post(c.dual(), val, ctx), // TODO?
            Pre(Seq(c1, c2), val, ctx) => Pre(*c1, val, Fst(box ctx, *c2)),
            Post(c1, val, Fst(ctx, c2)) => Pre(c2, val, Snd(c1, ctx)),
            Post(c2, val, Snd(c1, ctx)) => Post(Seq(box c1, box c2), val, *ctx),
            Pre(Sum(c1, c2), Val::Left(val), ctx) => Pre(*c1, *val, L_S(box ctx, *c2)),
            Post(c1, val, L_S(ctx, c2)) => Post(Sum(box c1, box c2), Val::Left(box val), *ctx),
            Pre(Sum(c1, c2), Val::Right(val), ctx) => Pre(*c2, *val, R_S(*c1, box ctx)),
            Post(c2, val, R_S(c1, ctx)) => Post(Sum(box c1, box c2), Val::Right(box val), *ctx),
            Pre(Prod(c1, c2), Val::Pair(v1, v2), ctx) => Pre(*c1, *v1, L_P(box ctx, *c2, *v2)),
            Post(c1, v1, L_P(ctx, c2, v2)) => Pre(c2, v2, R_P(c1, v1, ctx)),
            Post(c2, v2, R_P(c1, v1, ctx)) => Post(Prod(box c1, box c2), Val::Pair(box v1, box v2), *ctx),
            Post(c, v, ctx) => Post(c, v, ctx), // TODO halted here
            _ => panic!()
        }
    }
    pub fn dual(self) -> Self {
        use MachineState::*;
        use CombCtx::*;
        use Comb::*;

        match self {
            Pre(c, v, ctx) => Post(c.dual(), v, ctx),
            Post(c, v, ctx) => Pre(c.dual(), v, ctx),
        }
    }
}

fn main_run(idx: i32, s: MachineState, reverse: bool) -> (i32, MachineState) {
    println!("[{}] {:?}", idx, s);
    if let &MachineState::Post(_, _, CombCtx::Empty) = &s { 
        return (idx + 1, s) 
    }
    main_run(if reverse { idx - 1 } else { idx + 1 }, s.step(), reverse)
}

pub fn main(){
    let s_start = MachineState::Pre(
        grammar::comb("uniti; swap*; id").unwrap(),
        grammar::val("'foo").unwrap(),
        CombCtx::Empty
    );
    let (num_steps, s_end) = main_run(0, s_start, false);
    main_run(num_steps, s_end.dual(), true);
}

// pub fn main(){
//     let s = MachineState::Pre(
//         Comb::Seq(box Comb::Seq(box Comb::Iso(Iso::Uniti), box Comb::Iso(Iso::SwapP)), box Comb::Id),
//         Val::Symbol("foo".into()), 
//         CombCtx::Empty
//     );

//     fn main_step(idx: u32, s: MachineState){
//         println!("[{}] {:?}", idx, s);
//         if let &MachineState::Post(_, _, CombCtx::Empty) = &s { return }
//         main_step(idx + 1, s.step());
//     }
//     main_step(0, s);
// }

