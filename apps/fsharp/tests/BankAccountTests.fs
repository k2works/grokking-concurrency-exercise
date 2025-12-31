module BankAccountTests

open Xunit
open System.Threading
open GrokkingConcurrency.Ch08.BankAccount

[<Fact>]
let ``BankAccount deposits correctly`` () =
    let account = createAccount 100
    deposit account 50
    Assert.Equal(150, getBalance account)

[<Fact>]
let ``BankAccount withdraws correctly`` () =
    let account = createAccount 100
    let result = withdraw account 50
    Assert.True(result)
    Assert.Equal(50, getBalance account)

[<Fact>]
let ``BankAccount does not withdraw more than balance`` () =
    let account = createAccount 100
    let result = withdraw account 150
    Assert.False(result)
    Assert.Equal(100, getBalance account)

[<Fact>]
let ``BankAccount transfer is atomic`` () =
    let from = createAccount 100
    let toAccount = createAccount 0

    let result = transfer from toAccount 50
    Assert.True(result)
    Assert.Equal(50, getBalance from)
    Assert.Equal(50, getBalance toAccount)

[<Fact>]
let ``BankAccount handles concurrent transfers safely`` () =
    let account1 = createAccount 1000
    let account2 = createAccount 1000

    let latch = new CountdownEvent(100)

    let tasks =
        [ for _ in 1 .. 50 do
            yield async {
                transfer account1 account2 10 |> ignore
                latch.Signal() |> ignore
            }
            yield async {
                transfer account2 account1 10 |> ignore
                latch.Signal() |> ignore
            } ]

    tasks
    |> Async.Parallel
    |> Async.RunSynchronously
    |> ignore

    latch.Wait()

    // Total balance should be preserved
    Assert.Equal(2000, getBalance account1 + getBalance account2)
