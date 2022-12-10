module Common.Tests

open NUnit.Framework

[<Test>]
let create_ValidatesBehavior () =
    let r1 = Range.create 1 4
    Assert.That(r1.Start, Is.EqualTo(1))
    Assert.That(r1.End, Is.EqualTo(4))

    let r2 = Range.create 4 1
    Assert.That(r2.Start, Is.EqualTo(1))
    Assert.That(r2.End, Is.EqualTo(4))

[<Test>]
let order_ValidatesBehavior () =
    let r1 = { Start = 1; End = 4 }
    let or1 = Range.order r1
    Assert.That(or1.Start, Is.EqualTo(1))
    Assert.That(or1.End, Is.EqualTo(4))

    let r2 = { Start = 4; End = 1 }
    let or2 = Range.order r2
    Assert.That(or2.Start, Is.EqualTo(1))
    Assert.That(or2.End, Is.EqualTo(4))

[<Test>]
let Zero_ValidatesValues () =
    Assert.That(Range.Empty.Start, Is.EqualTo(0))
    Assert.That(Range.Empty.End, Is.EqualTo(0))

[<Test>]
let union_ValidatesValues () =
    let r1 = { Start = 1; End = 4 }
    let r2 = { Start = 10; End = 20 }
    let u1 = Range.union r1 r2
    Assert.That(u1.Start, Is.EqualTo(1))
    Assert.That(u1.End, Is.EqualTo(20))

[<Test>]
let containsValue_ValidatesBehavior () =
    let r1 = { Start = 1; End = 4 }
    Assert.That(Range.containsValue 0 r1, Is.False)
    Assert.That(Range.containsValue 1 r1, Is.True)
    Assert.That(Range.containsValue 3 r1, Is.True)
    Assert.That(Range.containsValue 4 r1, Is.True)
    Assert.That(Range.containsValue 5 r1, Is.False)

    let r2 = { Start = 1; End = 4 }
    Assert.That(Range.containsValue 0 r2, Is.False)
    Assert.That(Range.containsValue 1 r2, Is.True)
    Assert.That(Range.containsValue 3 r2, Is.True)
    Assert.That(Range.containsValue 4 r2, Is.True)
    Assert.That(Range.containsValue 5 r2, Is.False)

[<Test>]
let containsRange_ValidatesBehavior() =
    let r1 = { Start = 1; End = 4 }
    let r2 = { Start = -3; End = 0 }
    Assert.That(Range.containsRange r2 r1, Is.False)
    
    let r3 = { Start = 1; End = 4 }
    Assert.That(Range.containsRange r3 r1, Is.True)
    
    let r4 = {Start = 0; End = 3}
    Assert.That(Range.containsRange r1 r4, Is.False)
    
    let r5 = {Start = 4; End = 10}
    Assert.That(Range.containsRange r1 r5, Is.False)
    
    let r6 = {Start = 6; End = 10}
    Assert.That(Range.containsRange r1 r6, Is.False)
    
[<Test>]
let intersect_ValidatesBehavior() =
    let r1 = Range.create 0 2
    let r2 = Range.create 3 5
    match (Range.intersect r1 r2) with
    | Some _ -> Assert.Fail()
    | None -> Assert.Pass()
    
    let r3 = Range.create 2 4
    match (Range.intersect r1 r3) with
    | Some r ->
        Assert.That(r.Start, Is.EqualTo(2))
        Assert.That(r.End, Is.EqualTo(2))
    | None ->
        Assert.Fail()
        
    let r4 = Range.create 1 2
    match (Range.intersect r1 r4) with
    | Some r ->
        Assert.That(r.Start, Is.EqualTo(1))
        Assert.That(r.End, Is.EqualTo(1))
    | None ->
        Assert.Fail()
        
    let r5 = Range.create 0 2
    match (Range.intersect r1 r5) with
    | Some r ->
        Assert.That(r.Start, Is.EqualTo(0))
        Assert.That(r.End, Is.EqualTo(2))
    | None ->
        Assert.Fail()
        
    let r6 = Range.create -2 -1
    match (Range.intersect r1 r6) with
    | Some _ -> Assert.Fail()
    | None -> Assert.Pass()
    