module SMBolero.Server.Index

open Bolero.Html
open Bolero.Server.Html
open SMBolero

let page = doctypeHtml {
    head {
        meta { attr.charset "UTF-8" }
        meta { attr.name "viewport"; attr.content "width=device-width, initial-scale=1.0" }
        title { "Shader Minifier" }
        link { attr.rel "stylesheet"; attr.href "css/index.css" }
    }
    body {
        div { attr.id "main"; comp<Client.Main.MyApp> }
        boleroScript
    }
}
