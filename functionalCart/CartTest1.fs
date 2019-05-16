namespace Tests

open Cart
open NUnit.Framework
open FsUnit //$ dotnet add cart.fsproj package FSUnit

type TestClass () =

    let emptyCart: Cart= { Contents = [] }
    let chip = { Name = "Chips"; Price = 4.2<USD> ; Points= 14<PuffPoint> }
    let coke = { Name = "Coke"; Price = 1.6<USD>; Points = 14<PuffPoint> }

    let addChips cart =
        Add cart chip

    let addCoke cart =
        Add cart coke

    [<Test>]
    member this.MakeProducts () =
        let p = { Name = "Chips"; Price = 4.2<USD> ; Points= 14<PuffPoint> }
        Assert.Pass()

    [<Test>]
    member this.AddProducts () =
        let chip = { Name = "Chips"; Price = 4.2<USD> ; Points= 14<PuffPoint> }
        
        Add emptyCart chip 
        |> fun p -> p.Contents
        |> should haveLength 1
        
        Assert.Pass()

    [<Test>]
    member this.Add2OfTheSameProducts () =
        
        let expectedCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        addChips emptyCart 
             |> addChips
             |> addCoke
             |> should equal expectedCart

    [<Test>]
    member this.RemoveProductQuantity () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        let expectedCart = { emptyCart with Contents = [{Product = chip; Quantity = 1}
                                                        {Product = coke; Quantity = 1}]}                                                        

        Remove chip startingCart 
         |> should equal expectedCart

    [<Test>]
    member this.RemoveProductCompletly () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        let expectedCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}]}                                                        

        Remove chip startingCart
         |> Remove chip
         |> should equal expectedCart


    [<Test>]
    member this.UpdateCart () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        let expectedCart = { emptyCart with Contents = [{Product = coke; Quantity = 10}]}                                                        

        UpdateCart coke 9 startingCart
         |> UpdateCart chip -2
         |> should equal expectedCart         


   
    [<Test>]
    member this.CalculateCartTotal () =

        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        startingCart 
            |> CalculateTotal
            |> (fun c -> c.Total)
            |> formatCurrency
            |> should equal "5.80"


    [<Test>]
    member this.EnsureStockLevels () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        let c = startingCart |> CalculateTotal |> EnsureStockLevels
        
        match c with
            | GoodCart subtotalCart -> 
                subtotalCart.Total 
                    |> formatCurrency
                    |> should equal "5.80"
            | _ -> Assert.Fail("Didn't return a valid cart")


    [<Test>]
    member this.UpdateAndFinalize () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

        startingCart
        |> UpdateCartAndFinalize chip 3
        |> function
           | GoodCart subtotalCart -> 
                subtotalCart.Total 
                    |> formatCurrency
                    |> should equal "5.80"
            | _ -> Assert.Fail("Didn't return a valid cart")

    [<Test>]
    member this.UpdateAndFinalizeOOS () =
        let startingCart = { emptyCart with Contents = [{Product = coke; Quantity = 1}
                                                        {Product = chip; Quantity = 2}]}

         
        UpdateCartAndFinalize chip 30 startingCart
            |> function
                | Error outOfStockCart -> outOfStockCart.OutOfStock
                                            |> should haveLength 1
                | _ -> Assert.Fail("Got a valid cart when I should not have") 

        let outOfStockList =
            startingCart 
                |> UpdateCartAndFinalize chip 30 
                |> function
                    | Error outOfStockCart -> outOfStockCart.OutOfStock
                    | _ -> []  

        printf "%A" outOfStockList

        Assert.Pass()                                    