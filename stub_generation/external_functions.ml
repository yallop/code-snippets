let externals = Hashtbl.create 10
let register = Hashtbl.add externals
let resolve = Hashtbl.find externals
