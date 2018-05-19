# Development of a workflow driven biological graph analysis platform

This is a PoC application which is subject of my Master's thesis. I you are interested in the problem domain please refer to the extended abstract and presentations I prepared for the course Master Colloquium at the University of Applied Sciences Kufstein Tirol.

- [First presentation](https://1drv.ms/p/s!AmrMNl7mqRWGkcU7PauTghiEK-yazA)
- [Extended abstract](https://1drv.ms/b/s!AmrMNl7mqRWGkctxLhnl9DRKQDom3Q)
- [Second presentation](https://sway.com/Ef51d4prSoLU4zbH?ref=Link)

The application is provides as-is and MIT licensed. So just take the parts and ideas that work for you and use them to your liking. If you see things that appear strange or incorrect to you just open an issue. I'm very happy about your inputs and I'm looking forward to discussing it with you :)

## Preperation

You need a PostgreSQL database to run the application. The connection string is currently hard coded in the file `Api.fs` in the serverr project.

The client side application is currently only known to work well in a current version of Firefox. I know about strange behavior in Edge but don't have any knowledge on other browsers.

## Related work

The applicatin is written entirely in F#. It is a wonderful language to write complicated (and not so complicated) full stack applications and I encourage you to check it out. If you need a primer go [here](https://docs.microsoft.com/en-us/dotnet/fsharp/).

I built this application using the [SAFE Stack](https://safe-stack.github.io/) - a collection of technologies of concepts that are very well suited for writing web applications. You'll find [Saturn](https://github.com/SaturnFramework/Saturn) at the backend, [Fable Elmish](https://elmish.github.io/elmish/) in the front end (using [Fulma](https://mangelmaxime.github.io/Fulma/) to quickly build a nice UI) and [Marten FSharp](https://github.com/TheAngryByrd/Marten.FSharp) for data persistence.

The application is heavily inspered by the [SAFE bookstore](https://github.com/SAFE-Stack/SAFE-BookStore) and the [SAFE ConfPlanner](https://github.com/SAFE-Stack/SAFE-ConfPlanner).

For (right now rather basic) functional graph processing the F# port of the Haskell [FGL](https://hackage.haskell.org/package/fgl) library is used - which you can find [here](https://github.com/CSBiology/FSharp.FGL).
