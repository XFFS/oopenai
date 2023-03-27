(** {{1} Multipart Forms}

    This is a naive builder for multipart-form data. It does
    no validation and does not stream data. *)

module Part : sig
  type t
  (** The type of multipart-form parts *)

  val v : name:string -> ?filename:string -> ?typ:string -> string -> t
  (** [v ~name content] is a part with the given [name] containing
      supplied [content].

      Optional parameters

      - [filename]: Specify a filename for the part content
      - [typ]: Specify the content type for the part. This should be a
        valid MIME type. *)

  val add_to_buffer : Buffer.t -> t -> unit
  val to_string : t -> string
end

type t
(** The type of multipart forms *)

val v : ?boundary:string -> Part.t list -> t
(** [v parts] is a new multipart-form composed of the [parts].

    Optionally, a custom [boundary] marker can be supplied. Otherwise,
    a boundary is generated as a UUID. *)

val to_string : t -> string

val add_header : t -> Cohttp.Header.t -> Cohttp.Header.t
(** [add_header form headers] is [headers] with the appropriate
    multipart/form-data content type declaration added *)
