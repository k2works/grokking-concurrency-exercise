namespace GrokkingConcurrency.Ch02

open System.Security.Cryptography
open System.Text

module PasswordCracker =
    let private letters = "abcdefghijklmnopqrstuvwxyz"

    /// Generate all combinations of lowercase letters of the given length
    let getCombinations (length: int) : string list =
        let rec generate (current: string list) (remaining: int) : string list =
            if remaining = 0 then
                current
            else
                let next =
                    [ for prefix in current do
                      for letter in letters do
                          yield prefix + string letter ]
                generate next (remaining - 1)
        generate [""] length

    /// Calculate SHA-256 hash of the given password
    let getCryptoHash (password: string) : string =
        let bytes = Encoding.UTF8.GetBytes(password)
        let hash = SHA256.HashData(bytes)
        hash |> Array.map (fun b -> sprintf "%02x" b) |> String.concat ""

    /// Check if the password matches the given hash
    let checkPassword (password: string) (cryptoHash: string) : bool =
        getCryptoHash password = cryptoHash

    /// Crack password by brute force
    let crackPassword (cryptoHash: string) (length: int) : string option =
        getCombinations length
        |> List.tryFind (fun p -> checkPassword p cryptoHash)
