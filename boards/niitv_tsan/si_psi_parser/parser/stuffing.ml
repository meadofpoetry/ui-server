let name = "stuffing_descriptor"

let parse bs off = Bytes.parse ~offset:off bs "stuffing_byte"
