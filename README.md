# A Lattice Boltzmann implementation in F#

    cd src
    dotnet run

[LBM.fs](src/LBM.fs) implements the core logic of the method (the
maths, algorithm, and datatypes). It is short and fully commented,
one should be able to understand the gist of the Lattice Boltzmann method
by reading it.

[Genetic.fs](src/Genetic.fs) implements a simple genetic algorithm in order
to evolve an aerodynamic shell for recumbent bikes (velomobiles).
It depends on [LBM.fs](src/LBM.fs) to test shape prototypes in a virtual
2D wind tunnel.


[FluidSim.fs](src/FluidSim.fs) is the entry point of the program,
and is using [LBM.fs](src/LBM.fs) to setup and run the simulation,
as well as [Raylib-cs](https://github.com/ChrisDill/Raylib-cs)
to visualise it.

