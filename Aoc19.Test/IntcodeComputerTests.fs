module IntcodeComputerTests

open Xunit
open IntcodeComputer

[<Fact>]
let ``Program with one addition works`` () =
    let input = [| 1; 0; 0; 0; 99 |]
    let expexted = [| 2; 0; 0; 0; 99 |]

    let result = run input

    Assert.Equal<int array>(expexted, result)

[<Fact>]
let ``Program with one multiplication works`` () =
    let input = [| 2; 3; 0; 3; 99 |]
    let expexted = [| 2; 3; 0; 6; 99 |]

    let result = run input

    Assert.Equal<int array>(expexted, result)

[<Fact>]
let ``Program with two operations works`` () =
    let input = [| 1; 1; 1; 4; 99; 5; 6; 0; 99 |]
    let expexted = [| 30; 1; 1; 4; 2; 5; 6; 0; 99 |]

    let result = run input

    Assert.Equal<int array>(expexted, result)