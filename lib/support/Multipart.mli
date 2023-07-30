val part_of_file : name:string -> source:string -> filename:string -> Multipart_form.part Lwt.t
val part_of_string : name:string -> content:string -> Multipart_form.part
val form : ?header:Multipart_form.Header.t -> ?boundary:string -> Multipart_form.part list -> Multipart_form.multipart
