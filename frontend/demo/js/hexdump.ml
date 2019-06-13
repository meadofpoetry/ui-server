open Components

let section () =
  let data = "r2hdwksfbo423hedbwbadasdadjalksjdalkdjalkjdweruoncalndlqhdryhukabscorhjp;2hrouwesc;oihjkb4rfleaioaj.LNBEWGFOD;SAL/DNWERFHNSALDJDJpjnsdfouwhailfkdsfnwao;fhrti;hsldfacwaethgbvjsjkbt54mhrgfbdvsft54y6tjhngfbdvsetrhyjnfbg dfvsrethrynjfgbdfvsfyjtdhgnfgbdfvsyhjthnfgbdsdt5y4trhgdfvsee4t35egfvsdrgfefsdlkfjdsklfjalma,.fmdnmfklwnflskdf<gsdfg;sd" in
  let hexdump = Hexdump.make ~grouping:1 data () in
  Widget.create_div ~widgets:[hexdump] ()
