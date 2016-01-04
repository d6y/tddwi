data PowerSource = Petrol | Electric | Pedal

%default total

data Vehicle  : PowerSource -> Type where
  Bicycle     : Vehicle Pedal
  Unicycle    : Vehicle Pedal
  Motorcycle  : (fuel : Nat)   -> Vehicle Petrol
  ElectricCar : (charge : Nat) -> Vehicle Electric
  Car         : (fuel : Nat)   -> Vehicle Petrol
  Bus         : (fuel : Nat)   -> Vehicle Petrol

wheels : Vehicle power -> Nat
wheels Unicycle   = 1
wheels Bicycle    = 2
wheels Motorcycle = 2
wheels (Car fuel) = 4
wheels (Bus fuel) = 4
wheels (ElectricCar charge) = 4

refuel : Vehicle Petrol -> Vehicle Petrol
refuel (Motorcycle fuel) = Motorcycle 100
refuel (Car        fuel) = Car 100
refuel (Bus        fuel) = Bus 100
refuel Bicycle impossible
refuel Unicycle impossible
refuel (ElectricCar charge) impossible
