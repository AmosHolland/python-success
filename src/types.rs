#[derive(Clone, Debug)]
pub enum Type {
    Arrow(Box<Type>, Box<Type>),
    Num,
    Var(TypeVariable),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Arrow(t1, t2) => write!(f, "({0}) -> ({1})", *t1.to_owned(), *t2.to_owned()),
            Type::Num => write!(f, "Num"),
            Type::Var(type_variable) => write!(f, "{0}", type_variable.name),
        }
    }
}

#[derive(Clone, Debug)]
pub struct TypeVariable {
    pub id: usize,
    pub name: String,
}

pub struct TypeSubstitution {
    pub var: TypeVariable,
    pub new_type: Type,
}

impl std::fmt::Display for TypeSubstitution {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{0} |-> {1}", self.var.name, self.new_type)
    }
}
