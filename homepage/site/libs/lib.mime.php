<script language="PHP">
/*
 _|    _|            _|                              _|                _|
 _|    _|  _|_|_|        _|_|_|  _|_|      _|_|_|  _|_|_|_|  _|  _|_|      _|    _|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|_|      _|    _|_|
 _|    _|  _|    _|  _|  _|    _|    _|  _|    _|    _|      _|        _|  _|    _|
   _|_|    _|    _|  _|  _|    _|    _|    _|_|_|      _|_|  _|        _|  _|    _|

(c) 2008 Borg@sven-of-nine.de
*/

//////////////////////////////////////////////////////////////////////////////
/// Funtionen im Zusammenhang mit MimeTypes
//////////////////////////////////////////////////////////////////////////////

function mime_get_type($filename)
    {
    $types=array(

                 //Image
                 "bmp" =>"image/bmp",
                 "jpeg"=>"image/jpeg",
                 "ico" =>"image/ico",
                 "jpg" =>"image/jpg",
                 "gif" =>"image/gif",
                 "png" =>"image/png",

                 //Vanilla-text
                 "csv" =>"text/plain",
                 "log" =>"text/plain",
                 "text"=>"text/plain",
                 "txt" =>"text/plain",
                 "html"=>"text/html",
                 "htm" =>"text/html",
                 "xml" =>"text/xml",
                 "rss" =>"text/xhtml+xml",

                 //Application
                 "pdf" =>"application/x-pdf",
                 "xls" =>"application/ms-excel",
                 "doc" =>"application/msword",

                 //Compressed
                 "zip" =>"application/x-zip",
                 "rar" =>"application/x-rar",
                 "7z"  =>"application/x-7z",
                 "tar" =>"application/x-tar",

                 //Anwendung
                 "exe" =>"application/octet-stream",
                 "com" =>"application/octet-stream",

                 //Audio
                 "mp3" =>"audio/mpeg",
                 "wav" =>"audio/x-wav",

                 //Video
                 "mpeg"=>"video/mpeg",
                 "mpg" =>"video/mpeg",
                 "mpe" =>"video/mpeg",
                 "mov" =>"video/quicktime",
                 "avi" =>"video/x-msvideo"
                 );

    //Pfadinfos holen
    $ext=pathinfo(strtolower($filename));
    if (isset($types[$ext["extension"]]))
        {
        $result=$types[$ext["extension"]];
        }
    else
        {
        //OK, dann eben Default
        $result="application/octet-stream";
        }
    return($result);
    }
</script>