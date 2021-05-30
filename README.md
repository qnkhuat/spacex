# Auto docking Dragon Simulation
Why spend 2 minutes to try to dock the ship when you can spend 1 day to program it dock automatically in 3m12s?

Notes about this version:
- Implemented in Lisp
- Interact with Browser by calling RESTAPI to chromedriver
- Control algorithm: purely proportional with some tweaks. Will try PID later

# To run
- Install sbcl
- Download ChromeDriver and run it on port 9515
- Run command: `sbcl --load spacex.lisp` to start the program
