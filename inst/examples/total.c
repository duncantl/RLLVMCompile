double
Sum1(double *x)
{
    int i;
    double total = 0;
    for(i = 0; i < 10; i++)
	total = total + 1;
    return(total);
} 


double
Sum3(int len)
{
    int i;
    double total = 0;
    while(i < len) {
	i = i + 1;
	total = total + 1;
    }
    return(total);
} 
