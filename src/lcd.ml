
(*  Initialize SPI driver with LCD.
    -> ml_spi_max_transfer_size: Maximum number of bytes transferred in one SPI command.
*)
external init : int -> unit = "caml_lcd_init"

(*
    Output DMA addressable buffer buffer to spi lcd.
    -> ml_buffer: Cstruct.t buffer allocated by `caml_lcd_alloc_buffer`
    -> ml_x -> ml_y: Coordinates where the buffer will be printed on.
    -> ml_width -> ml_height: Dimensions of the buffer. 
*)
external write_buffer : Cstruct.buffer -> int -> int -> int -> int -> unit = "caml_lcd_write_buffer"

(*
    Blocks and returns only when the lcd spi buffer can be written to.
*)
external wait_spi_write : unit -> unit = "caml_lcd_wait_spi_write"

(*
    Allocate a buffer suitable for LCD transmission. (the buffer must reside in DMA addressable memory)
    -> ml_size: size of the buffer, in pixels.
*)
external alloc_buffer : int -> Cstruct.buffer = "caml_lcd_alloc_buffer"

(* Width of the screen. *)
let width = 320
(* Height of the screen*)
let height = 240

(* Temporary video buffer size in pixels. *)
let buffer_size = 512
(* Maximum transfer in one SPI command. *)
let max_spi_transfer = 1024

(* Temporary video buffer, for fill/draw commands *)
let video_buffer = Cstruct.of_bigarray (alloc_buffer buffer_size)


let () = init max_spi_transfer

(*  Fills a video buffer with a given 16-bit value.
    Warning: no bound check. *)
let rec fill_buffer buffer size value =
    match size with 
    | 0 ->  ()
    | n ->  Cstruct.BE.set_uint16 buffer (2*(n-1)) value; 
            fill_buffer buffer (n-1) value

(*  Fill internal buffer with a color. *)
let fill_internal_buffer = fill_buffer video_buffer

(*  Transmit the given video buffer, cutting the transmission into multiple SPI transfers if the request
    is too big. 
    -> x -> y: screen destination coordinates. 
    -> width -> height: buffer dimensions.*)
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

(*  Fill a rectangle of a given 16-bit color on the screen. 
    -> x -> y: screen destination coordinates.
    -> width -> height: rectangle dimensions.
    -> color: 16-bit color written as 0brrrrrggggggbbbbb. (5 bits red + 6 bits green + 5 bits blue). *)
let fill_rect x y width height color =
    fill_internal_buffer (min buffer_size (width*height)) color;
    let n_lines_per_transfer = max_spi_transfer / (2 * width) in
    let rec partial_transmit y target_y len =
        if y + n_lines_per_transfer > target_y then
            begin
                if target_y > y then 
                    begin
                        write_buffer (Bigarray.Array1.sub video_buffer.Cstruct.buffer 0 len) x y width (target_y - y);
                        wait_spi_write ();
                    end
                else ();
            end
        else
            begin
                write_buffer (Bigarray.Array1.sub video_buffer.Cstruct.buffer 0 (2*width*n_lines_per_transfer)) x y width n_lines_per_transfer;
                wait_spi_write ();
                partial_transmit (y+n_lines_per_transfer) target_y (len - width*n_lines_per_transfer*2)
            end
    in
    partial_transmit y (y+height) (width*height*2)

(*  Draw a colored 1-pixel wide horizontal line.
    -> y: y-position
    -> x_begin -> x_end: interval in which the line is drawn
    -> color: 16-bit color *)
let draw_Hline y x_begin x_end color = 
    let width = x_end - x_begin in 
    fill_internal_buffer width color;
    write_buffer video_buffer.Cstruct.buffer x_begin y width 1;
    wait_spi_write ()

(*  Draw a colored 1-pixel wide vertical line.
    -> x: x-position
    -> y_begin -> y_end: interval in which the line is drawn
    -> color: 16-bit color *)
let draw_Vline x y_begin y_end color = 
    let height = y_end - y_begin in 
    fill_internal_buffer height color;
    write_buffer video_buffer.Cstruct.buffer x y_begin 1 height;
    wait_spi_write ()
