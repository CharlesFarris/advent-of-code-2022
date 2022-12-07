open System.IO

let input =
    ".\\part1_data.txt"
    |> File.ReadAllLines
    |> Seq.toList

type File =
    { DirectoryName: string
      Name: string
      Size: int }

type Directory =
    { Name: string
      ParentName: string
      Depth: int }

type State =
    { Directories: Map<string, Directory>
      Files: File list
      CurrentDirectory: string }

let handleChangeDirectory (name: string) (state: State) : State =
    match name with
    | ".." ->
        { state with
            CurrentDirectory =
                state.Directories[state.CurrentDirectory]
                    .ParentName }
    | _ ->
        { state with
            CurrentDirectory =
                match state.CurrentDirectory with
                | "" -> name
                | "/" -> sprintf "%s%s" state.CurrentDirectory name
                | _ -> sprintf "%s/%s" state.CurrentDirectory name }

let handleListDirectory (state: State) : State = state

let addDirectory (name: string) (state: State) : State =
    let depth =
        match state.CurrentDirectory with
        | "" -> 0
        | _ ->
            state.Directories[state.CurrentDirectory].Depth
            + 1

    let directory =
        { Name =
            if state.CurrentDirectory = "/" then
                sprintf "/%s" name
            else
                sprintf "%s/%s" state.CurrentDirectory name
          ParentName = state.CurrentDirectory
          Depth = depth }

    { state with Directories = state.Directories.Add(directory.Name, directory) }

let addFile (size: string) (name: string) (state: State) : State =
    let file =
        { DirectoryName = state.CurrentDirectory
          Name =
            if state.CurrentDirectory = "/" then
                sprintf "/%s" name
            else
                sprintf "%s/%s" state.CurrentDirectory name
          Size = int size }

    { state with Files = (state.Files |> List.append [ file ]) }

let parseLine (state: State) (line: string) : State =
    printfn "%s" line
    let tokens = line.Split [| ' ' |]

    match tokens[0] with
    | "$" ->
        match tokens[1] with
        | "cd" -> state |> handleChangeDirectory tokens[2]
        | "ls" -> state |> handleListDirectory
        | _ -> state
    | "dir" -> state |> addDirectory tokens[1]
    | _ -> state |> addFile tokens[0] tokens[1]

let initialState: State =
    { Directories =
        Map.empty.Add(
            "/",
            { Name = "/"
              ParentName = ""
              Depth = 0 }
        )
      Files = []
      CurrentDirectory = "" }

let finalState =
    List.fold parseLine initialState input

let computeDirectorySize (name: string) (state: State) : int =
    List.fold
        (fun sum file ->
            if file.DirectoryName = name then
                sum + file.Size
            else
                sum)
        0
        state.Files

let sortedDirectories =
    finalState.Directories.Values
    |> Seq.toList
    |> List.sortBy (fun directory -> -directory.Depth)

let directorySizes: Map<string, int> =
    List.fold
        (fun map directory -> map.Add(directory, computeDirectorySize directory finalState))
        Map.empty
        (sortedDirectories
         |> List.map (fun directory -> directory.Name))

let updateParentDirectorySize (map: Map<string, int>) (directory: Directory) =
    map.Change(
        directory.ParentName,
        (fun v ->
            match v with
            | Some x -> Some(map[directory.ParentName] + map[directory.Name])
            | None -> None)
    )

let totalDirectorySizes =
    List.fold updateParentDirectorySize directorySizes sortedDirectories

let sum =
    totalDirectorySizes.Values
    |> Seq.toList
    |> List.filter (fun size -> size <= 100000)
    |> List.sum

let totalDiskSpace = 70000000
let requiredSpace = 30000000

let currentSpace =
    totalDiskSpace - totalDirectorySizes["/"]

let neededSpace =
    requiredSpace - currentSpace

let minimumDirectorySize =
    totalDirectorySizes.Values
    |> Seq.toList
    |> List.filter (fun size -> neededSpace <= size)
    |> List.min

printfn "Directory Size: %i" minimumDirectorySize