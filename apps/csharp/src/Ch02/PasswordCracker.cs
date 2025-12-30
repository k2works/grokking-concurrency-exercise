using System.Security.Cryptography;
using System.Text;

namespace GrokkingConcurrency.CSharp.Ch02;

/// <summary>
/// パスワードクラッカー（逐次処理版）
/// ブルートフォース（総当たり）でパスワードを解読する
/// </summary>
public static class PasswordCracker
{
    /// <summary>
    /// 指定された桁数のパスワード組み合わせを生成
    /// </summary>
    /// <param name="length">パスワードの桁数</param>
    /// <param name="minNumber">最小値</param>
    /// <param name="maxNumber">最大値</param>
    /// <returns>パスワード候補のリスト</returns>
    public static List<string> GetCombinations(int length, int minNumber, int maxNumber)
    {
        var combinations = new List<string>();
        var format = new string('0', length);

        for (var i = minNumber; i <= maxNumber; i++)
        {
            combinations.Add(i.ToString(format));
        }

        return combinations;
    }

    /// <summary>
    /// パスワードのSHA-256ハッシュを計算
    /// </summary>
    /// <param name="password">パスワード</param>
    /// <returns>SHA-256ハッシュ（16進数文字列）</returns>
    public static string GetCryptoHash(string password)
    {
        var bytes = Encoding.UTF8.GetBytes(password);
        var hash = SHA256.HashData(bytes);
        return Convert.ToHexString(hash).ToLowerInvariant();
    }

    /// <summary>
    /// パスワードが一致するか確認
    /// </summary>
    /// <param name="expectedCryptoHash">期待されるハッシュ</param>
    /// <param name="possiblePassword">候補パスワード</param>
    /// <returns>一致すればtrue</returns>
    public static bool CheckPassword(string expectedCryptoHash, string possiblePassword)
    {
        var actualCryptoHash = GetCryptoHash(possiblePassword);
        return expectedCryptoHash.Equals(actualCryptoHash);
    }

    /// <summary>
    /// ブルートフォースでパスワードを解読
    /// </summary>
    /// <param name="cryptoHash">パスワードのハッシュ</param>
    /// <param name="length">パスワードの桁数</param>
    /// <returns>解読されたパスワード、見つからなければnull</returns>
    public static string? CrackPassword(string cryptoHash, int length)
    {
        var maxNumber = (int)Math.Pow(10, length) - 1;
        var combinations = GetCombinations(length, 0, maxNumber);

        foreach (var combination in combinations)
        {
            if (CheckPassword(cryptoHash, combination))
            {
                return combination;
            }
        }

        return null;
    }
}
