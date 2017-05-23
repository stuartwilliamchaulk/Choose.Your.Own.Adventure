// Learn more about F# at http://fsharp.org
open System

type Option = {
    Text: string;
    NextPrompt: int;
    Key: string;
    
}

type Prompt ={
    Text: string;
    Option1: Option option;
    Option2: Option option;
    Retry: bool;
    End: bool;
}

let prompts (promptId: int) : Prompt =
    match promptId with
    | 1 -> 
        {  
            Text = "You have walked into a conference.  Around you, you see nerds, hippies and ravers (who are also all nerds).";
            Option1 = Some({ Text = "Go right, into a room with a placard saying 'A categorical view of computational effects'"; NextPrompt = 2; Key="Right"});
            Option2 = Some({ Text = "Go around the corner, where you catch the smell of suits"; NextPrompt = 3; Key="corner"});
            Retry = false;
            End = false
        }
    | 2 -> 
        {
            Text = "You sit down and are given a lecture about category theory.  You understand almost nothing. \nYou are the dummest person in the room, possible ever.";
            Option1 = None;
            Option2 = None;
            Retry = false;
            End = false
        }
    | 3 ->
        {
            Text = "An open area is before you.  A long table is festooned with treats of every kind, including muffins.  \nIn the far corner is a musty desk covered in literature.";
            Option1 = Some({ Text = "Approch the table laden with yummies"; NextPrompt = 4; Key="table"});
            Option2 = Some({ Text = "<sigh> Go peruse the literature at the desk, maybe"; NextPrompt = 5; Key="desk" });
            Retry = false;
            End = false
        }
    | 4 -> 
        {
            Text = "You start cramming muffins in your muffin hole beside the suits.  One of them is your boss.\n 'Jensen', he says, assuming thats your name.  You clearly came here to eat and drink for free and not become wise in the ways of kung-foo.";
            Option1 = None;
            Option2 = None;
            Retry = false;
            End = false
        }
    | 5 -> 
        {
            Text = "On the table is a stack of books. Freebies!  They are large musty tomes entitled 'F# for fun and profit'. It has no pictures except for line diagrams and things like Q-> fn(x). \nThere is also a pamphlet with pretty pictures called 'Just what the heck is functional programming?'.";
            Option1 = Some({ Text = "Read the boring book that would only really be of interest to computer nerds (the worst type of nerd)"; NextPrompt = 6; Key="read"});
            Option2 = Some({ Text = "Skim the pamphlet so you can pretend to know what functional programming is among other people who are not nerds"; NextPrompt = 7; Key="skim" });
            Retry = false;
            End = false
        }
    | 6 -> 
        {
            Text = "You reach the introduction. Boy, that sure is coderish.  \nYou think to yourself: am I really interested in a deep dive in l33t g33k?";
            Option1 = Some({ Text = "Yep."; NextPrompt = 100; Key="y"});
            Option2 = Some({ Text = "Nope"; NextPrompt = 7; Key="n" });
            Retry = false;
            End = false
        }
    | 7 -> 
        {
            Text = "You vaguely pay attention to some guy talking about shite.";
            Option1 = Some({ Text = "Is he done yet."; NextPrompt = 8; Key="done"});
            Option2 = Some({ Text = "Really, not yet"; NextPrompt = 8; Key="yet"});
            Retry = false;
            End = false
        }
    | 8 -> 
        {
            Text = "'Thank god that's over', you say to yourself.\n You stretch, and walk further into the building. \nYou reach an exit, but notice out of the corner of your eye a mousehole with a door, and a bottle labeled 'drink me'.";
            Option1 = Some({ Text = "Leave via the exit"; NextPrompt = 9; Key="exit"});
            Option2 = Some({ Text = "Drink the potion"; NextPrompt = 10; Key="drink"});
            Retry = false;
            End = false
        }
    | 9 -> 
        {
            Text = "You open the exit and walk out.  Suddenly all goes black.";
            Option1 = None;
            Option2 = None;
            Retry = false;
            End = false
        }
    | 10 ->
        {
            Text = "You quickly gulp back the oily concoction, gagging as you do so.  Guess what? Yep, you shink.\n You can now see that the mouse door, currently the correct size for you, has a sign saying 'Micoservices ahead'.";
            Option1 =Some({ Text = "'Screw that, I'm outta here' you think, and leave"; NextPrompt = 11; Key="leave"});
            Option2 =Some({ Text = "Whoohoo, sounds interesting, head in"; NextPrompt = 100; Key="in"});
            Retry = false;
            End = false
        }
    | 11 ->
        {
            Text = "You head back to the exit, but are small.  Someone steps on you.  And guess what.";
            Option1 =None;
            Option2 =None;
            Retry = false;
            End = false
        }
    | 100 ->
        {
            Text = "You asked for it.  Now listen to the nerd.";
            Option1 = None;
            Option2 = None;
            Retry = false;
            End = true
        }
    | _ ->
        {
            Text = "I don't know what you said.  Try again, genius.";
            Option1 = None;
            Option2 = None;
            Retry = true;
            End = false
        }

[<EntryPoint>]
let main argv = 
    
    printfn "The CONFERENCE"
    printfn "A BlueMetal Choose Your Own Adventure Story"
    printfn "-------------------------------------------"
    Console.CursorVisible <- true

    let toLower (string: string) =
        string.ToLower()

    let contains (phrase: string) (key: string): bool=
        phrase.Contains(key)

    let parseResult (option1: Option) (option2: Option) (phrase: string) =
        let match1 = contains phrase (toLower option1.Key)
        let match2 = contains phrase (toLower option2.Key)
        if (match1) then Some(option1)
        else if match2 then Some(option2)
        else None

    let rec showPrompt (prompt: Prompt, previousPrompt: Prompt) =
        printfn ""
        printfn "%s" prompt.Text
        match prompt.Option1 with
        | Some opt ->
            printfn "Would you like to:"
            printfn "-%s?" opt.Text
            match prompt.Option2 with
            | Some opt2 ->
                printfn "-%s?" opt2.Text 
            | _ -> ()
            let answer = Console.ReadLine()
            let nextOption = parseResult opt prompt.Option2.Value answer
            match nextOption with
            | None -> 
                let newPrompt  = prompts 0
                showPrompt(newPrompt, prompt)
            | Some nextOption -> 
                let newPrompt = prompts nextOption.NextPrompt
                showPrompt(newPrompt,prompt)
           
        | _ ->
            if (prompt.Retry) then
                showPrompt(previousPrompt, prompt)
            else if (prompt.End) then
                ()
            else
                printf "You have been eaten by a grue."
        ()
    
    let firstPrompt = prompts 1
    showPrompt(firstPrompt,firstPrompt)
    let junk = Console.ReadKey()
    0 // return an integer exit code
