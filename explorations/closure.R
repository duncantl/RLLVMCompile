library(RLLVMCompile)

account =
function(balance = 0L)
{
    deposit = function(amt)
                  balance <<- balance + amt
    withdrawl = function(amt)
                    balance <<- balance - amt

    list(deposit = deposit, withdraw = withdrawl,  balance = function() balance)
}

A = account()
B = account()
A$balance()
A$deposit(100)
A$deposit(50)
B$deposit(50)
A$withdraw(35)
B$withdraw(78)
A$balance()
B$balance()

