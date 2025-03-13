-- RAM / Cores / <ScreenSize>
data PC  = Tower Integer Integer | Laptop Integer Integer Double

macbook = Laptop 8 4 14.2
server = Tower 16 8


stats :: PC -> String
stats (Tower ram cores) = "Tower:  RAM=" ++ show ram ++ "GB Cores=" ++ show cores 
stats (Laptop ram cores screen) = "Laptop:  RAM=" ++ show ram ++ "GB Cores=" ++ show cores ++ " Screensize=" ++ show screen 






