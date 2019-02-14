open Ctypes

module C (F : Ctypes.FOREIGN) = struct
  
  let test_fun = F.(foreign "test_fun" (int @-> returning int))

end
