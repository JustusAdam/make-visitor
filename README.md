# Create visitors from your type definitions

A comprehensive example:

These are our definitions

```rs
pub struct Expr(Box<ExprValue>);

pub struct Ident(String);

pub struct BigInt(u64);

pub enum ExprValue {
    AddressOf(Expr),
    Array {
        elems: Vec<Expr>,
    },
    ArrayOf {
        elem: Expr,
    },
    Assign {
        left: Expr,
        right: Expr,
    },
    BinOp {
        op: BinaryOperator,
        lhs: Expr,
        rhs: Expr,
    },
    Dereference(Expr),
    DoubleConstant(f64),
    EmptyUnion,
    FloatConstant(f32),
    FunctionCall {
        function: Expr,
        arguments: Vec<Expr>,
    },
    If {
        c: Expr,
        t: Expr,
        e: Expr,
    },
    Index {
        array: Expr,
        index: Expr,
    },
    IntConstant(BigInt),
    Member {
        lhs: Expr,
        field: Ident,
    },
    PointerConstant(u64),
    SelfOp {
        op: SelfOperator,
        e: Expr,
    },
    StatementExpression {
        statements: Vec<Stmt>,
    },
    StringConstant {
        s: Ident,
    },
    Struct {
        values: Vec<Expr>,
    },
    Symbol {
        identifier: Ident,
    },
    Typecast(Expr),
    Union {
        value: Expr,
        field: Ident,
    },
    UnOp {
        op: UnaryOperator,
        e: Expr,
    },
    Vector {
        elems: Vec<Expr>,
    },
}

pub enum BinaryOperator {
    And,
    Ashr,
    Bitand,
    Bitor,
    Bitnand,
    Bitxor,
    Div,
    Equal,
    Ge,
    Gt,
    IeeeFloatEqual,
    IeeeFloatNotequal,
    Implies,
    Le,
    Lshr,
    Lt,
    Minus,
    Mod,
    Mult,
    Notequal,
    Or,
    OverflowMinus,
    OverflowMult,
    OverflowPlus,
    OverflowResultMinus,
    OverflowResultMult,
    OverflowResultPlus,
    Plus,
    ROk,
    Rol,
    Ror,
    Shl,
    VectorEqual,
    VectorNotequal,
    VectorGe,
    VectorGt,
    VectorLe,
    VectorLt,
    Xor,
}

pub enum SelfOperator {
    Postdecrement,
    Postincrement,
    Predecrement,
    Preincrement,
}

pub enum UnaryOperator {
    Bitnot,
    BitReverse,
    Bswap,
    IsDynamicObject,
    IsFinite,
    Not,
    ObjectSize,
    PointerObject,
    PointerOffset,
    UnaryMinus,
}

pub struct Stmt {
    body: Box<StmtBody>,
}

pub enum StmtBody {
    AssignStmt {
        lhs: Expr,
        rhs: Expr,
    },
    AtomicBlock(Vec<Stmt>),
    Block(Vec<Stmt>),
    Break,
    Continue,
    Decl {
        lhs: Expr,
        value: Option<Expr>,
    },
    Deinit(Expr),
    Expression(Expr),
    For {
        init: Stmt,
        cond: Expr,
        update: Stmt,
        body: Stmt,
    },
    FunctionCall {
        lhs: Option<Expr>,
        function: Expr,
        arguments: Vec<Expr>,
    },
    Goto(Ident),
    Ifthenelse {
        i: Expr,
        t: Stmt,
        e: Option<Stmt>,
    },
    Label {
        label: Ident,
        body: Stmt,
    },
    Return(Option<Expr>),
    Skip,
    Switch {
        control: Expr,
        cases: Vec<SwitchCase>,
        default: Option<Stmt>,
    },
    While {
        cond: Expr,
        body: Stmt,
    },
}

pub struct SwitchCase {
    case: Expr,
    body: Stmt,
}
```

And for that the `make_visitor!` macro creates the following:

```rs
mod visit {
    use super::{
        BigInt, BinaryOperator, Expr, ExprValue, Ident, SelfOperator, Stmt, StmtBody, SwitchCase,
        UnaryOperator,
    };
    use std::borrow::Borrow;
    pub trait Visitor {
        fn visit_expr(&mut self, i: &Expr) {
            visit_expr(self, i)
        }
        fn visit_ident(&mut self, i: &Ident) {
            visit_ident(self, i)
        }
        fn visit_big_int(&mut self, i: &BigInt) {
            visit_big_int(self, i)
        }
        fn visit_expr_value(&mut self, i: &ExprValue) {
            visit_expr_value(self, i)
        }
        fn visit_binary_operator(&mut self, i: &BinaryOperator) {
            visit_binary_operator(self, i)
        }
        fn visit_self_operator(&mut self, i: &SelfOperator) {
            visit_self_operator(self, i)
        }
        fn visit_unary_operator(&mut self, i: &UnaryOperator) {
            visit_unary_operator(self, i)
        }
        fn visit_stmt(&mut self, i: &Stmt) {
            visit_stmt(self, i)
        }
        fn visit_stmt_body(&mut self, i: &StmtBody) {
            visit_stmt_body(self, i)
        }
        fn visit_switch_case(&mut self, i: &SwitchCase) {
            visit_switch_case(self, i)
        }
    }
    pub fn visit_expr<V: Visitor + ?Sized>(v: &mut V, i: &Expr) {
        v.visit_expr_value((&i.0).borrow())
    }
    pub fn visit_ident<V: Visitor + ?Sized>(v: &mut V, i: &Ident) {}
    pub fn visit_big_int<V: Visitor + ?Sized>(v: &mut V, i: &BigInt) {}
    pub fn visit_expr_value<V: Visitor + ?Sized>(v: &mut V, i: &ExprValue) {
        match i {
            ExprValue::AddressOf(field_0) => v.visit_expr(field_0),
            ExprValue::Array { elems } => {
                for it in (elems).iter() {
                    v.visit_expr(it)
                }
            }
            ExprValue::ArrayOf { elem } => v.visit_expr(elem),
            ExprValue::Assign { left, right } => {
                v.visit_expr(left);
                v.visit_expr(right)
            }
            ExprValue::BinOp { op, lhs, rhs } => {
                v.visit_binary_operator(op);
                v.visit_expr(lhs);
                v.visit_expr(rhs)
            }
            ExprValue::Dereference(field_0) => v.visit_expr(field_0),
            ExprValue::DoubleConstant(field_0) => {}
            ExprValue::EmptyUnion => (),
            ExprValue::FloatConstant(field_0) => {}
            ExprValue::FunctionCall {
                function,
                arguments,
            } => {
                v.visit_expr(function);
                for it in (arguments).iter() {
                    v.visit_expr(it)
                }
            }
            ExprValue::If { c, t, e } => {
                v.visit_expr(c);
                v.visit_expr(t);
                v.visit_expr(e)
            }
            ExprValue::Index { array, index } => {
                v.visit_expr(array);
                v.visit_expr(index)
            }
            ExprValue::IntConstant(field_0) => v.visit_big_int(field_0),
            ExprValue::Member { lhs, field } => {
                v.visit_expr(lhs);
                v.visit_ident(field)
            }
            ExprValue::PointerConstant(field_0) => {}
            ExprValue::SelfOp { op, e } => {
                v.visit_self_operator(op);
                v.visit_expr(e)
            }
            ExprValue::StatementExpression { statements } => {
                for it in (statements).iter() {
                    v.visit_stmt(it)
                }
            }
            ExprValue::StringConstant { s } => v.visit_ident(s),
            ExprValue::Struct { values } => {
                for it in (values).iter() {
                    v.visit_expr(it)
                }
            }
            ExprValue::Symbol { identifier } => v.visit_ident(identifier),
            ExprValue::Typecast(field_0) => v.visit_expr(field_0),
            ExprValue::Union { value, field } => {
                v.visit_expr(value);
                v.visit_ident(field)
            }
            ExprValue::UnOp { op, e } => {
                v.visit_unary_operator(op);
                v.visit_expr(e)
            }
            ExprValue::Vector { elems } => {
                for it in (elems).iter() {
                    v.visit_expr(it)
                }
            }
        }
    }
    pub fn visit_binary_operator<V: Visitor + ?Sized>(v: &mut V, i: &BinaryOperator) {
        match i {
            BinaryOperator::And => (),
            BinaryOperator::Ashr => (),
            BinaryOperator::Bitand => (),
            BinaryOperator::Bitor => (),
            BinaryOperator::Bitnand => (),
            BinaryOperator::Bitxor => (),
            BinaryOperator::Div => (),
            BinaryOperator::Equal => (),
            BinaryOperator::Ge => (),
            BinaryOperator::Gt => (),
            BinaryOperator::IeeeFloatEqual => (),
            BinaryOperator::IeeeFloatNotequal => (),
            BinaryOperator::Implies => (),
            BinaryOperator::Le => (),
            BinaryOperator::Lshr => (),
            BinaryOperator::Lt => (),
            BinaryOperator::Minus => (),
            BinaryOperator::Mod => (),
            BinaryOperator::Mult => (),
            BinaryOperator::Notequal => (),
            BinaryOperator::Or => (),
            BinaryOperator::OverflowMinus => (),
            BinaryOperator::OverflowMult => (),
            BinaryOperator::OverflowPlus => (),
            BinaryOperator::OverflowResultMinus => (),
            BinaryOperator::OverflowResultMult => (),
            BinaryOperator::OverflowResultPlus => (),
            BinaryOperator::Plus => (),
            BinaryOperator::ROk => (),
            BinaryOperator::Rol => (),
            BinaryOperator::Ror => (),
            BinaryOperator::Shl => (),
            BinaryOperator::VectorEqual => (),
            BinaryOperator::VectorNotequal => (),
            BinaryOperator::VectorGe => (),
            BinaryOperator::VectorGt => (),
            BinaryOperator::VectorLe => (),
            BinaryOperator::VectorLt => (),
            BinaryOperator::Xor => (),
        }
    }
    pub fn visit_self_operator<V: Visitor + ?Sized>(v: &mut V, i: &SelfOperator) {
        match i {
            SelfOperator::Postdecrement => (),
            SelfOperator::Postincrement => (),
            SelfOperator::Predecrement => (),
            SelfOperator::Preincrement => (),
        }
    }
    pub fn visit_unary_operator<V: Visitor + ?Sized>(v: &mut V, i: &UnaryOperator) {
        match i {
            UnaryOperator::Bitnot => (),
            UnaryOperator::BitReverse => (),
            UnaryOperator::Bswap => (),
            UnaryOperator::IsDynamicObject => (),
            UnaryOperator::IsFinite => (),
            UnaryOperator::Not => (),
            UnaryOperator::ObjectSize => (),
            UnaryOperator::PointerObject => (),
            UnaryOperator::PointerOffset => (),
            UnaryOperator::UnaryMinus => (),
        }
    }
    pub fn visit_stmt<V: Visitor + ?Sized>(v: &mut V, i: &Stmt) {
        v.visit_stmt_body((&i.body).borrow())
    }
    pub fn visit_stmt_body<V: Visitor + ?Sized>(v: &mut V, i: &StmtBody) {
        match i {
            StmtBody::AssignStmt { lhs, rhs } => {
                v.visit_expr(lhs);
                v.visit_expr(rhs)
            }
            StmtBody::AtomicBlock(field_0) => {
                for it in (field_0).iter() {
                    v.visit_stmt(it)
                }
            }
            StmtBody::Block(field_0) => {
                for it in (field_0).iter() {
                    v.visit_stmt(it)
                }
            }
            StmtBody::Break => (),
            StmtBody::Continue => (),
            StmtBody::Decl { lhs, value } => v.visit_expr(lhs),
            StmtBody::Deinit(field_0) => v.visit_expr(field_0),
            StmtBody::Expression(field_0) => v.visit_expr(field_0),
            StmtBody::For {
                init,
                cond,
                update,
                body,
            } => {
                v.visit_stmt(init);
                v.visit_expr(cond);
                v.visit_stmt(update);
                v.visit_stmt(body)
            }
            StmtBody::FunctionCall {
                lhs,
                function,
                arguments,
            } => {
                v.visit_expr(function);
                for it in (arguments).iter() {
                    v.visit_expr(it)
                }
            }
            StmtBody::Goto(field_0) => v.visit_ident(field_0),
            StmtBody::Ifthenelse { i, t, e } => {
                v.visit_expr(i);
                v.visit_stmt(t)
            }
            StmtBody::Label { label, body } => {
                v.visit_ident(label);
                v.visit_stmt(body)
            }
            StmtBody::Return(field_0) => {}
            StmtBody::Skip => (),
            StmtBody::Switch {
                control,
                cases,
                default,
            } => {
                v.visit_expr(control);
                for it in (cases).iter() {
                    v.visit_switch_case(it)
                }
            }
            StmtBody::While { cond, body } => {
                v.visit_expr(cond);
                v.visit_stmt(body)
            }
        }
    }
    pub fn visit_switch_case<V: Visitor + ?Sized>(v: &mut V, i: &SwitchCase) {
        v.visit_expr(&i.case);
        v.visit_stmt(&i.body)
    }
}
```