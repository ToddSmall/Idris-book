data PowerSource = Electric | Pedal | Petrol

data Vehicle : PowerSource -> Type where
     Unicycle : Vehicle Pedal
     Bicycle : Vehicle Pedal
     Motorcycle : (fuel : Nat) -> Vehicle Petrol
     Car : (fuel : Nat) -> Vehicle Petrol
     Bus : (fuel : Nat) -> Vehicle Petrol
     Tram : Vehicle Electric

wheels : Vehicle power -> Nat
wheels Unicycle = 1
wheels Bicycle = 2
wheels (Motorcycle fuel) = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels Tram = 8

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 25
refuel (Car fuel) = Car 100
refuel (Bus fuel) = Car 200
refuel Unicycle impossible
refuel Bicycle impossible
refuel Tram impossible

