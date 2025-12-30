using GrokkingConcurrency.CSharp.Ch08;

namespace GrokkingConcurrency.CSharp.Tests.Ch08;

/// <summary>
/// 銀行口座のテスト（同期と排他制御）
/// </summary>
public class BankAccountTests
{
    [Fact]
    public void ShouldDeposit()
    {
        var account = new BankAccount(100);

        account.Deposit(50);

        Assert.Equal(150, account.Balance);
    }

    [Fact]
    public void ShouldWithdraw()
    {
        var account = new BankAccount(100);

        var result = account.Withdraw(50);

        Assert.True(result);
        Assert.Equal(50, account.Balance);
    }

    [Fact]
    public void ShouldNotWithdrawWithInsufficientFunds()
    {
        var account = new BankAccount(100);

        var result = account.Withdraw(150);

        Assert.False(result);
        Assert.Equal(100, account.Balance);
    }

    [Fact]
    public void ShouldMaintainBalanceUnderConcurrentAccess()
    {
        var account = new BankAccount(1000);
        const int numThreads = 100;
        const int depositsPerThread = 10;
        using var countdown = new CountdownEvent(numThreads);

        var tasks = Enumerable.Range(0, numThreads)
            .Select(_ => Task.Run(() =>
            {
                for (var j = 0; j < depositsPerThread; j++)
                {
                    account.Deposit(1);
                }
                countdown.Signal();
            }))
            .ToArray();

        countdown.Wait(TimeSpan.FromSeconds(10));
        Task.WaitAll(tasks);

        // 1000 + (100 * 10) = 2000
        Assert.Equal(2000, account.Balance);
    }

    [Fact]
    public void ShouldTransferBetweenAccounts()
    {
        var from = new BankAccount(100);
        var to = new BankAccount(50);

        var result = BankAccount.Transfer(from, to, 30);

        Assert.True(result);
        Assert.Equal(70, from.Balance);
        Assert.Equal(80, to.Balance);
    }
}
