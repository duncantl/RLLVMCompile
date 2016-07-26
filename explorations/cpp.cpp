
double foo(int i)
{
    return(i + 1.0);
}

double foo(double i)
{
    return(i + 2.5);
}


class A {
  
protected:
    int val;
 
public:
    A(int v) : val(v) { };

    int bar() { return(val);}
    int bar(int x) { val += x; return(val);}

};


int
zoo(int n)
{
    A a(n);
    a.bar();
    a.bar(3);
    return(a.bar());
}
