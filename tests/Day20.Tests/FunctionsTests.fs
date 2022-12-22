module Day20.Tests

open NUnit.Framework

[<Test>]
let testGetValueAt () =
    let arrangement = [11; 22; 33; 44]
    Assert.That(arrangement |> Functions.getValueAt 0, Is.EqualTo(11))
    // positive indices
    Assert.That(arrangement |> Functions.getValueAt 1, Is.EqualTo(22))
    Assert.That(arrangement |> Functions.getValueAt 2, Is.EqualTo(33))
    Assert.That(arrangement |> Functions.getValueAt 3, Is.EqualTo(44))
    Assert.That(arrangement |> Functions.getValueAt 4, Is.EqualTo(11))
    Assert.That(arrangement |> Functions.getValueAt 5, Is.EqualTo(22))
    Assert.That(arrangement |> Functions.getValueAt 6, Is.EqualTo(33))
    Assert.That(arrangement |> Functions.getValueAt 7, Is.EqualTo(44))
    Assert.That(arrangement |> Functions.getValueAt 8, Is.EqualTo(11))
    // negative indices
    Assert.That(arrangement |> Functions.getValueAt -1, Is.EqualTo(44))
    Assert.That(arrangement |> Functions.getValueAt -2, Is.EqualTo(33))
    Assert.That(arrangement |> Functions.getValueAt -3, Is.EqualTo(22))
    Assert.That(arrangement |> Functions.getValueAt -4, Is.EqualTo(11))
    Assert.That(arrangement |> Functions.getValueAt -5, Is.EqualTo(44))
    Assert.That(arrangement |> Functions.getValueAt -6, Is.EqualTo(33))
    Assert.That(arrangement |> Functions.getValueAt -7, Is.EqualTo(22))
    Assert.That(arrangement |> Functions.getValueAt -8, Is.EqualTo(11))

[<Test>]
let testMix () =
    let arrangement = [1; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 1
    CollectionAssert.AreEqual(newArrangement, [22; 1; 33; 44])

    let arrangement = [2; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 2
    CollectionAssert.AreEqual(newArrangement, [22; 33; 2; 44])
    
    let arrangement = [3; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 3
    CollectionAssert.AreEqual(newArrangement, [22; 33; 44; 3])

    let arrangement = [22; 33; 3; 44]
    let newArrangement = arrangement |> Functions.mix 3
    CollectionAssert.AreEqual(newArrangement, [22; 33; 3; 44])
    
    let arrangement = [4; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 4
    CollectionAssert.AreEqual(newArrangement, [4; 22; 33; 44])
    
    let arrangement = [5; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 5
    CollectionAssert.AreEqual(newArrangement, [22; 5; 33; 44])

    let arrangement = [6; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix 6
    CollectionAssert.AreEqual(newArrangement, [22; 33; 6; 44])
    
    let arrangement = [22; 4; 33; 44]
    let newArrangement = arrangement |> Functions.mix 4
    CollectionAssert.AreEqual(newArrangement, [22; 4; 33; 44])

    let arrangement = [22; 33; 4; 44]
    let newArrangement = arrangement |> Functions.mix 4
    CollectionAssert.AreEqual(newArrangement, [22; 33; 4; 44])

    let arrangement = [22; 33; 44; 4]
    let newArrangement = arrangement |> Functions.mix 4
    CollectionAssert.AreEqual(newArrangement, [22; 33; 44; 4])

    let arrangement = [1; 22; 33; 44; 55]
    let newArrangement = arrangement |> Functions.mix 1
    CollectionAssert.AreEqual(newArrangement, [22; 1; 33; 44; 55])

    let arrangement = [22; 1; 33; 44; 55]
    let newArrangement = arrangement |> Functions.mix 1
    CollectionAssert.AreEqual(newArrangement, [22; 33; 1; 44; 55])

    let arrangement = [22; 33; 44; 1; 55]
    let newArrangement = arrangement |> Functions.mix 1
    CollectionAssert.AreEqual(newArrangement, [22; 33; 44; 55; 1])

    let arrangement = [22; 33; 44; 55; 1]
    let newArrangement = arrangement |> Functions.mix 1
    CollectionAssert.AreEqual(newArrangement, [22; 1; 33; 44; 55])
    
    let arrangement = [-1; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix -1
    CollectionAssert.AreEqual(newArrangement, [22; 33; -1; 44])
    
    let arrangement = [-2; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix -2
    CollectionAssert.AreEqual(newArrangement, [22; -2; 33; 44])

    let arrangement = [-3; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix -3
    CollectionAssert.AreEqual(newArrangement, [-3 ; 22; 33; 44])

    let arrangement = [-4; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix -4
    CollectionAssert.AreEqual(newArrangement, [22; 33; 44; -4])

    let arrangement = [-5; 22; 33; 44]
    let newArrangement = arrangement |> Functions.mix -5
    CollectionAssert.AreEqual(newArrangement, [22; 33; -5; 44])
