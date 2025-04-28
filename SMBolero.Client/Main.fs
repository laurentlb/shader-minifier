module SMBolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Templating.Client
open ShaderMinifier

type Page =
    | Home
    | FlagPage
    | About

type Flags = {
    mutable format: string
    mutable inlining: bool
    mutable removeUnused: bool
    mutable renaming: bool
    mutable other: string
}

/// The Elmish application's model.
type Model =
    {
        page: Page
        shaderInput: string
        shaderSize: int
        flags: Flags
        error: string option
    }

let initModel =
    {
        page = Home
        shaderInput = "out vec4 fragColor;\nvoid main() {\n  fragColor = vec4(1.,1.,1.,1.);\n}"
        shaderSize = 0
        flags = {Flags.inlining=true; removeUnused=true; renaming=true; other=""; format="text"}
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Minify
    | SetShader of string
    | Error of exn
    | ClearError

let minify flags content =
    let options = Minifier.ParseOptions(flags)
    let minifier = Minifier(options, [|"input", content|])
    let out = new System.IO.StringWriter()
    minifier.Format(out)

    let withLoc = new System.IO.StringWriter()
    minifier.FormatWithLocations(withLoc)

    out.ToString(), minifier.GetSize, withLoc.ToString()

module API =
    [<Microsoft.JSInterop.JSInvokableAttribute("minify")>]
    let minify flags content =
        let result, _, _ = minify flags content
        result

let update (jsRuntime: Microsoft.JSInterop.IJSRuntime) message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | Minify ->
        let allFlags = [|
            yield! ["--format"; model.flags.format]
            if not model.flags.inlining then yield "--no-inlining"
            if not model.flags.removeUnused then yield "--no-remove-unused"
            if not model.flags.renaming then yield "--no-renaming"
            yield! model.flags.other.Split([|' '|], System.StringSplitOptions.RemoveEmptyEntries)
        |]
        printfn "Minify %s" (String.concat " " allFlags)
        try
            let out, size, withLoc = minify allFlags model.shaderInput
            jsRuntime.InvokeAsync("updateShader", [|model.shaderInput; withLoc|]) |> ignore
            { model with shaderSize = size }, Cmd.none
        with
            | e ->
                printfn "Error: %s" e.Message
                jsRuntime.InvokeAsync("updateShader", [|model.shaderInput; e.Message|]) |> ignore
                { model with shaderSize = 0 }, Cmd.none
    | SetShader value ->
        { model with shaderInput = value }, Cmd.none
    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router: Router<Page, Model, Message> =
    { getEndPoint = fun model -> model.page
      getRoute = function
          | Home -> "?main"
          | About -> "?about"
          | FlagPage -> "?flags"
      setRoute = function
          | "?main" -> Some Home
          | "?about" -> Some About
          | "?flags" -> Some FlagPage
          | _ -> None
      makeMessage = SetPage
      notFound = Some Home }

type Main = Template<"wwwroot/main.html">

let aboutPage model dispatch =
    Main.About()
        .Elt()

let flagPage model dispatch =
    Main.FlagPage()
        .FlagHelp(Options.flagsHelp.Value)
        .Elt()

let homePage model dispatch =
    Main.Home()
        .Minify(fun _ -> dispatch Minify)
        .ShaderInput(model.shaderInput, fun v -> dispatch (SetShader v))
        .ShaderSize(if model.shaderSize = 0 then "" else $"generated, size: {model.shaderSize}")
        .Format(model.flags.format, fun v -> model.flags.format <- v)
        .Inlining(model.flags.inlining, fun v -> model.flags.inlining <- v)
        .RemoveUnused(model.flags.removeUnused, fun v -> model.flags.removeUnused <- v)
        .Renaming(model.flags.renaming, fun v -> model.flags.renaming <- v)
        .OtherFlags(model.flags.other, fun v -> model.flags.other <- v)
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat {
            menuItem model Home "Minifier"
            menuItem model FlagPage "Flags"
            menuItem model About "About"
        })
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | About -> aboutPage model dispatch
            | FlagPage -> flagPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty()
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg (Message.SetPage Home)) (update this.JSRuntime) view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
