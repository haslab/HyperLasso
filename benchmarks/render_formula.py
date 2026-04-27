import sys
import itertools

class Formula:
    def __init__(self, children=None, label=None, dim=None, isLTL=False):
        self.children = children or []
        self.label = label
        self.dim = dim
        self.isLTL = isLTL
        self.dims = {dim} if dim else set()

    # overloading primitive operators
    def __and__(self, other): return Binary("(", self, "&", other, ")")
    def __or__(self, other):  return Binary("(", self, "|", other, ")")
    # we only use isLTL for AH atomic propositions, which do not accept "->"
    def __rshift__(self, other): return Binary("(", self, "->", other, ")",isLTL=True)
    def __eq__(self, other): return Binary("(", self, "=", other, ")")
    def __ne__(self, other): return Binary("(", self, "!=", other, ")")
    def __invert__(self): return Unary("(!", self,")")

    def prettyprint(self, indent=0, ah=False):
        print("\n".join(self.render(indent,ah,True)))

    def render(self, indent=0, ah=False,ahbrackets=False):
        if ah and ahbrackets and (not self.isLTL or isinstance(self, Terminal)) and len(self.dims)>0:
            t = "\t" * indent
            lines = []
            lines.append(f"{t}{{")
            lines.extend(self.renderAux(indent+1,ah,False))
            lines.append(f"{t}}}")
            return lines
        else:
            return self.renderAux(indent,ah,ahbrackets)

    def renderAux(self, indent=0, ah=False, ahbrackets=False):
        t = "\t" * indent
        t1 = "\t" * (indent + 1)
        if isinstance(self, Terminal):
            if self.dim:
                return [f"{t}\"{self.label}\"_{self.dim}" if ah else f"{t}{self.label}[{self.dim}]"]
            else: 
                return [f"{t}{self.label}" if ah else f"{t}{self.label}"]
        
        if isinstance(self, Binary):
            lines = [f"{t}{self.open_b}"]
            lines.extend(self.left.render(indent + 1,ah,ahbrackets))
            lines.append(f"{t1}{self.op}")
            lines.extend(self.right.render(indent + 1,ah,ahbrackets))
            lines.append(f"{t}{self.close_b}")
            return lines

        if isinstance(self, Multi):
            if not self.children:
                ss = self.default.lower() if ah else self.default
                return [t + ss]
            lines = [f"{t}("]
            for j, child in enumerate(self.children):
                lines.extend(child.render(indent + 1,ah,ahbrackets))
                if j < len(self.children) - 1:
                    lines.append(f"{t1}{self.op}")
            lines.append(f"{t})")
            return lines

        if isinstance(self, Unary):
            return [f"{t}{self.prefix}"] + self.child.render(indent + 1,ah,ahbrackets) + [f"{t}{self.suffix}"]

        if isinstance(self, Quantifier):
            return [f"{t}{self.q} {self.var}."] + self.child.render(indent,ah,ahbrackets)

class Terminal(Formula): pass

class Binary(Formula):
    def __init__(self, open_b, left, op, right, close_b,isLTL=False):
        super().__init__(isLTL=isLTL)
        self.open_b, self.left, self.op, self.right, self.close_b = open_b, left, op, right, close_b
        self.isLTL |= left.isLTL | right.isLTL
        self.dims |= left.dims | right.dims
class Multi(Formula):
    def __init__(self, children, op, default,isLTL=False):
        super().__init__(isLTL=isLTL)
        self.children, self.op, self.default = children, op, default
        self.isLTL |= any([ child.isLTL for child in children ])
        for child in children:
            self.dims |= child.dims
class Unary(Formula):
    def __init__(self, prefix, child, suffix,isLTL=False):
        super().__init__(isLTL=isLTL)
        self.prefix, self.child, self.suffix = prefix, child, suffix
        self.isLTL |= child.isLTL
        self.dims |= child.dims
class Quantifier(Formula):
    def __init__(self, q, var, child,isLTL=False):
        super().__init__(isLTL=isLTL)
        self.q, self.var, self.child = q, var, child
        self.isLTL |= child.isLTL
        self.dims |= child.dims

def v(s,dim=None,isLTL=False): return Terminal(label=s,dim=dim,isLTL=isLTL)
def ands(xs,isLTL=False): return Multi(xs, "&", "TRUE",isLTL=isLTL)
def ors(xs,isLTL=False):  return Multi(xs, "|", "FALSE",isLTL=isLTL)
def G(x): return Unary("(G", x, ")",isLTL=True)
def F(x): return Unary("(F", x, ")",isLTL=True)
def X(x): return Unary("(X", x, ")",isLTL=True)
def exists(v, x): return Quantifier("exists", v, x,isLTL=True)
def forall(v, x): return Quantifier("forall", v, x,isLTL=True)

def atomic(x,dim=None): return v(x,dim=dim,isLTL=True)
def const(x): return v(str(x))

def equiv(x, y): return Binary("(", x, "<->", y, ")",isLTL=True)

def existsN(v,n,x):
    def existsi(i):
        if i < n: return exists(f"{v}{i}",existsi(i+1))
        else: return x
    return existsi(0)

def forallN(v,n,x):
    def foralli(i):
        if i < n: return forall(f"{v}{i}",foralli(i+1))
        else: return x
    return foralli(0)
        

