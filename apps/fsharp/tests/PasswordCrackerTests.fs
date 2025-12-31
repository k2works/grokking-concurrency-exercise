module PasswordCrackerTests

open Xunit
open GrokkingConcurrency.Ch02.PasswordCracker

[<Fact>]
let ``getCombinations generates all combinations of given length`` () =
    let combinations = getCombinations 2
    Assert.Contains("aa", combinations)
    Assert.Contains("az", combinations)
    Assert.Contains("za", combinations)
    Assert.Contains("zz", combinations)
    Assert.Equal(676, List.length combinations) // 26 * 26

[<Fact>]
let ``getCryptoHash returns SHA-256 hash`` () =
    let hash = getCryptoHash "ab"
    Assert.Equal("fb8e20fc2e4c3f248c60c39bd652f3c1347298bb977b8b4d5903b85055620603", hash)

[<Fact>]
let ``checkPassword returns true for matching hash`` () =
    let hash = getCryptoHash "test"
    Assert.True(checkPassword "test" hash)

[<Fact>]
let ``checkPassword returns false for non-matching hash`` () =
    let hash = getCryptoHash "test"
    Assert.False(checkPassword "wrong" hash)

[<Fact>]
let ``crackPassword finds password for given hash`` () =
    let password = "ab"
    let hash = getCryptoHash password
    Assert.Equal(Some password, crackPassword hash 2)

[<Fact>]
let ``crackPassword returns None for non-existent password`` () =
    let hash = "0000000000000000000000000000000000000000000000000000000000000000"
    Assert.Equal(None, crackPassword hash 2)
