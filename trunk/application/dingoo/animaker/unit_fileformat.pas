unit unit_fileformat;

interface
uses unit_typedefs;

var
aAniHeader : array[0..47] of Byte =
    (
    $41, $4e, $49, $31, $40,
    $01, $00, $00, $f0, $00, $00, $00, $10,
    $00, $00, $00, $01, $00, $00, $00, $01,
    $00, $00, $00,
    $76, $72, $69, $78, $2e, $79, $61, $6e,
    $40, $01,
    $00, $00,
    $f0, $00,
    $00, $00, $00, $00, $00, $00,
    $e8, $03,
    $00, $00
    );

implementation


end.
