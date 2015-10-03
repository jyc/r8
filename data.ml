module Entry = struct
  type t = {
    name : string;
    message : string;
  }
end

module Database = struct
  type t = {
    mutable records : Entry.t list
  }
end
