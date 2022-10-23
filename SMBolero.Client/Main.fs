module SMBolero.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// Routing endpoints definition.
type Page =
    | [<EndPoint "?main">] Home
    | [<EndPoint "?flags">] FlagPage
    | [<EndPoint "?help">] Help

/// The Elmish application's model.
type Model =
    {
        page: Page
        shaderInput: string
        shaderOutput: string
        flags: string
        error: string option
    }

let initModel =
    {
        page = Home
        shaderInput = "int foo() { return 6 * 7; }"
        shaderOutput = ""
        flags = "--format text"
        error = None
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | Minify
    | SetShader of string
    | SetFlags of string
    | Error of exn
    | ClearError

let minify flags content =
    try
        Options.init flags
        let shaders, exportedNames = ShaderMinifier.minify [|"input", content|]
        let out = new System.IO.StringWriter()
        Formatter.print out shaders exportedNames Options.Globals.options.outputFormat
        out.ToString()
    with
        | e -> e.Message

let update message model =
    match message with
    | SetPage page ->
        { model with page = page }, Cmd.none
    | Minify ->
        printfn "Minify %s" model.flags
        let out = minify (model.flags.Split(' ')) model.shaderInput
        { model with shaderOutput = out }, Cmd.none
    | SetShader value ->
        { model with shaderInput = value }, Cmd.none
    | SetFlags value ->
        { model with flags = value }, Cmd.none

    | Error exn ->
        { model with error = Some exn.Message }, Cmd.none
    | ClearError ->
        { model with error = None }, Cmd.none

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

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
        .ShaderOutput(model.shaderOutput)
        .Flags(model.flags, fun v -> dispatch (SetFlags v))
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
            menuItem model Help "Help"
        })
        .Body(
            cond model.page <| function
            | Home -> homePage model dispatch
            | Help -> aboutPage model dispatch
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
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg (Message.SetPage Home)) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
