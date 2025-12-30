using System.Runtime.CompilerServices;

namespace GrokkingConcurrency.CSharp.Ch08;

/// <summary>
/// 銀行口座 - 同期と排他制御の例
/// Lock を使ったスレッドセーフな実装
/// </summary>
public class BankAccount
{
    private readonly object _lock = new();
    private int _balance;

    public BankAccount(int initialBalance)
    {
        _balance = initialBalance;
    }

    /// <summary>
    /// 残高
    /// </summary>
    public int Balance
    {
        get
        {
            lock (_lock)
            {
                return _balance;
            }
        }
    }

    /// <summary>
    /// 入金
    /// </summary>
    public void Deposit(int amount)
    {
        lock (_lock)
        {
            _balance += amount;
        }
    }

    /// <summary>
    /// 出金
    /// </summary>
    public bool Withdraw(int amount)
    {
        lock (_lock)
        {
            if (_balance >= amount)
            {
                _balance -= amount;
                return true;
            }
            return false;
        }
    }

    /// <summary>
    /// 口座間送金（デッドロック回避付き）
    /// </summary>
    public static bool Transfer(BankAccount from, BankAccount to, int amount)
    {
        // デッドロック回避のため、常に同じ順序でロックを取得
        var first = RuntimeHelpers.GetHashCode(from) < RuntimeHelpers.GetHashCode(to) ? from : to;
        var second = first == from ? to : from;

        lock (first._lock)
        {
            lock (second._lock)
            {
                if (from._balance >= amount)
                {
                    from._balance -= amount;
                    to._balance += amount;
                    return true;
                }
                return false;
            }
        }
    }
}
