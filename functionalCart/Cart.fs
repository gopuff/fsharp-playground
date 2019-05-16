module Cart

[<Measure>] 
type USD
[<Measure>] 
type PuffPoint

type Product = {
    Name: string
    Price: float<USD>
    Points: int<PuffPoint>
}

type CartItem = { 
    Product: Product
    Quantity: int
    }

type Cart = {
    Contents: List<CartItem> 
}
let multiply x y = 
    x * y

let consolidateCart(cart: Cart) =  
    let newContents =
        cart.Contents 
        |> Seq.groupBy(fun (i: CartItem) -> i.Product) 
        |> Seq.map (fun (key, values) -> 
            { Product= key; Quantity = values|> Seq.sumBy (fun (i : CartItem) -> i.Quantity) })
        |> Seq.toList
    { cart with Contents= newContents }

let Add (cart: Cart) (product: Product): Cart =
    let newCartItems = { Product= product; Quantity= 1} :: cart.Contents
    consolidateCart({ cart with Contents = newCartItems })


// Remove
let filterCart cartItems: List<CartItem> =  
        cartItems   
            |> Seq.groupBy(fun (i: CartItem) -> i.Product) 
            |> Seq.map (fun (key, values) -> 
                { Product= key; Quantity = values|> Seq.sumBy (fun (i : CartItem) -> i.Quantity) })
            |> Seq.filter (fun cartItem -> cartItem.Quantity > 0)
            |> Seq.toList

let consolidateCartWithRemove(cart: Cart) =  
    let newContents =
        cart.Contents 
        |> filterCart

    { cart with Contents= newContents }

// reversed arguments for currying
let Remove (product: Product) (cart: Cart): Cart =
    let newCartItems = { Product= product; Quantity= -1} :: cart.Contents
    consolidateCartWithRemove({ cart with Contents = newCartItems })

let rec UpdateCart (product: Product) (quantity: int) (cart: Cart): Cart =
    match quantity with
    | var1 when var1 = 0 -> cart
    | var1 when var1 > 0 -> UpdateCart (product) (quantity-1) (Add cart product)
    | var1 when var1 < 0 -> UpdateCart (product) (quantity+1) (Remove product cart)
    | _ -> cart


// complete the cart

type SubtotalCart = {
    Contents: List<CartItem>
    Total: float<USD>
}

type OutOfStockCart = {
    Contents: List<CartItem>
    Total: float<USD>
    OutOfStock: List<CartItem>
}

type CustomerCart = 
    | GoodCart of SubtotalCart
    | Error of OutOfStockCart

let CheckStockByName (name: string) = 
    match name with
    | "Coke" -> 1
    | _ -> 10

let CheckStock (cartItem) =
    CheckStockByName cartItem.Product.Name >= cartItem.Quantity 

let floorCurrency (value: float<USD>): float<USD> =
    System.Math.Round (value / 1.0<USD> ,2) * 1.0<USD>

let formatCurrency (value: float<USD>): string =
    sprintf "%.2f" (value / 1.0<USD>)

let CalculateTotal (cart: Cart): SubtotalCart = 
    let total: float<USD> =
        cart.Contents 
        |>Seq.sumBy (fun (i : CartItem) -> i.Product.Price)
        |>floorCurrency

    { Contents=cart.Contents; Total= total}
    

let EnsureStockLevels (cart: SubtotalCart): CustomerCart = 
    let inStock, outOfStock = List.partition CheckStock cart.Contents
    match outOfStock with
    | [] -> GoodCart {cart with Contents = inStock}
    | _ ->  Error { Contents = inStock; Total= cart.Total; OutOfStock = outOfStock}


let FinalizeCart = CalculateTotal >> EnsureStockLevels

let UpdateCartAndFinalize (product: Product) (quantity: int) (cart: Cart) = 
    UpdateCart product quantity cart
    |> FinalizeCart