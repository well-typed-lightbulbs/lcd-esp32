
external init : int -> unit = "caml_lcd_init"
external write_buffer : Cstruct.buffer -> int -> int -> int -> int -> unit = "caml_lcd_write_buffer"
external wait_spi_write : unit -> unit = "caml_lcd_wait_spi_write"
external alloc_buffer : int -> Cstruct.buffer = "caml_lcd_alloc_buffer"

let width = 320
let height = 240
(* video buffer size in 2-bytes unit *)
let buffer_size = 1024
let max_spi_transfer = 2048

let video_buffer = Cstruct.of_bigarray (alloc_buffer buffer_size)
let buffer_color = ref 0x0000
let buffer_len   = ref 0

let () = init max_spi_transfer

let rec fill_buffer buffer size value =
    match size with 
    | 0 ->  ()
    | n ->  Cstruct.BE.set_uint16 buffer (2*(n-1)) value; 
            fill_buffer buffer (n-1) value

let fill_internal_buffer = fill_buffer video_buffer


let transmit_buffer buffer x y width height = 
    let n_lines_per_transfer = max_spi_transfer / (2 * width) in
    let rec partial_transmit y target_y ofs len =
        if y + n_lines_per_transfer > target_y then
            begin
                if target_y > y then 
                    begin
                        write_buffer (Bigarray.Array1.sub buffer.Cstruct.buffer ofs len) x y width (target_y - y);
                        wait_spi_write ();
                    end
                else ();
            end
        else
            begin
                write_buffer (Bigarray.Array1.sub buffer.Cstruct.buffer ofs len) x y width n_lines_per_transfer;
                wait_spi_write ();
                partial_transmit (y+n_lines_per_transfer) target_y (ofs + width*n_lines_per_transfer*2) (len - width*n_lines_per_transfer*2)
            end
    in
    partial_transmit y (y+height) 0 (width*height*2)


let fill_rect x y width height color =
    fill_internal_buffer (min buffer_size (width*height)) color;
    transmit_buffer video_buffer x y width height

let draw_Hline y x_begin x_end color = 
    let width = x_end - x_begin in 
    fill_internal_buffer width color;
    write_buffer video_buffer.Cstruct.buffer x_begin y width 1;
    wait_spi_write ()

let draw_Vline x y_begin y_end color = 
    let height = y_end - y_begin in 
    fill_internal_buffer height color;
    write_buffer video_buffer.Cstruct.buffer x y_begin 1 height;
    wait_spi_write ()
