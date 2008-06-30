<script language="PHP">
////////////////////////////////////////////////////////////////////////////////////
///
/// Klassen zur einfachen Erzeugung von Charts
///
////////////////////////////////////////////////////////////////////////////////////
/*
$chart=new piechart();

$r1=rand(5,10);
for ($r=0; $r < $r1; $r++)
    {
    $chart->setlimit(10,$r);
    $chart->setrowname(crc32($r),$r);

    $xpos=0;
    $ypos=0;
    for ($x=0; $x < 1; $x++)
        {
        $chart->add($x,rand(5000,40000),$r);
        }
    }

$chart->font=2;
$chart->showlegend=TRUE;
$chart->width=640;
$chart->height=480;
$chart->title="Tabu Blong Smoke Insaid";
$chart->background="C:/DevWeb/Kimba/_ima1ges/pool/alu_blue.png";
$chart->path="c:/bopfen/";
$chart->image="dogosch.png";

$chart->execute();
$chart->save(FALSE,"web");
$chart->destroy();
*/
////////////////////////////////////////////////////////////////////////////////////
//
//Die Basisklasse für die Datenaufbereitung
//
//Sie umfasst alle grundlegenden Funktionen zur Erzeugung der Grafiken,
// der Farbtabellen und der Datenaufnahme.
//
//Public-Eigenschaften
// image  = Name der Grafik, die mit Save erzeugt wird
// title  = Titel der Grafik
// xtitle = Achsenbeschriftung
// ytitle = Achsenbeschriftung
//Private-Eigenschaften
// internal_data       = array(array());  Puffer für die Daten Aufbau : array[row][xpos]=ypos
// internal_color      = array();         Array für Farben Aufbau : array[row]=color
// internal_rownames   = array();         Array für Reihennamen Aufbau array[row]=text
// internal_handle     =FALSE;            Grafikhandle
// internal_background = FALSE;           Pfad zur Hintergrunddatei
// internal_path       = FALSE;           Speicherpfad für die Grafik
// internal_limit      = array();         Reihengröße für Push
// internal_font       = 2;               Font für Beschriftungen
// internal_processor  = FALSE;           Zeiger auf registrierten Grafikprozessor
//
//Methoden
// destroy()             Destruktor
// setdefaults()         Setzt auf Standardwerte zurück und löscht alle Daten
// setlimit(row=0)       Setzt ein Datensatzlimit für den Push-Befehl
// getlimit(row)         Holt das gesetzte Limit für Datenreihe "row"
// setrowname(name,row)  Setzt einen Namen für Datenreihe "row" => für die Legende
// execute()             Erzeugt die Grafik
// registerprocess(func) Registriert einen eigenen Grafikprozessor (Nur in vererbten Klassen)
// save(file=FALSE)      Speichert die erzeugte Grafik ab. Wird kein Name angegeben, wird ein
//                       Name vergeben und kann unter ->image abgefragt werden
// add(x,y,row=0)        Daten zufügen. x und y können auch Arrays sein
// push(y,row=0)         Daten pushen. Die Reihe enthält nie mehr als mit setlimit angegebene Werte
////////////////////////////////////////////////////////////////////////////////////
class chartbase
    {
    //Properties
    
    //Pfad für fertige Grafik
    var $path       = FALSE;

    //Dateiname des erzeugten Bildes
    var $image      = FALSE;

    //Hintergrund JA/NEIN
    var $background = FALSE;

    //Beschriftungen
    var $title      = "";
    var $xtitle     = "";
    var $ytitle     = "";
    
    var $showlegend = TRUE;
    //Fontgröße
    var $font       = 2;

    //Größe
    var $width      = 640;
    var $height     = 480;

    //Abstände zu den Rändern
    var $top    = 10;
    var $bottom = 10;
    var $left   = 10;
    var $right  = 10;

    //Min/Max-Werte (Sind erst nach einem Execute gesetzt)
    var $xmin      = 0;
    var $xmax      = 0;
    var $ymin      = 0;
    var $ymax      = 0;
    var $xspan     = 0;
    var $yspan     = 0;

    //Pixelbox in der gezeichnet wird
    var $boxleft   = 0;
    var $boxright  = 0;
    var $boxtop    = 0;
    var $boxbottom = 0;
    var $boxwidth  = 0;
    var $boxheigth = 0;

    //FontMetrics
    var $fontwidth  = 0;
    var $fontheight = 0;


    //////////////////////////////////////////////////
    //Intern
    //Die Datenarrays
    var $internal_data   = array(array());

    //Farbarray
    var $internal_color  = array();

    //Legendentextarray
    var $internal_rownames = array();
    
    //Image-Handle
    var $internal_handle=FALSE;
    

    //Maximale Reihenlänge bei eine Push
    var $internal_limit      = array();
    
    //Hier kann eine vererbte Funktion registriert werde, die
    //von Execute aufgerufen wird
    var $internal_processor  = FALSE;

    //////////////////////////////////////////////////
    //Konstruktor
    function chartbase()
        {
        $this->setdefaults();
        }

    //////////////////////////////////////////////////
    //Destruktor
    function destroy()
        {
        if ($this->internal_handle!=FALSE)
            {
            imagedestroy($this->internal_handle);
            }
        }

    //////////////////////////////////////////////////
    //Defaulteinstellungen
    function setdefaults()
        {
        $this->showlegend        = FALSE;
        $this->background        = FALSE;
        $this->internal_data     = array(array());
        $this->internal_color    = array();
        $this->internal_rownames = array();
        $this->internal_handle   = FALSE;
        $this->internal_limit    =array_fill(0,32,50);
        $this->title             = "";
        $this->xtitle            = "";
        $this->ytitle            = "";
        $this->width             = 640;
        $this->height            = 480;
        $this->font     = 2;
        
        //Den Defaultprozessor registrieren
        $this->registerprocessor("_processor");
        }
    
     //////////////////////////////////////////////////
     //Maximale Größe der Datenreihen angeben
     //Wird nur bei einem PUSH beachtet
     function setlimit($limit,$row=0)
        {
        if ($limit>0)
            {
            $this->internal_limit[$row]=$limit;
            }
        }
        
     //////////////////////////////////////////////////
     //Gesetztes Limit lesen
     function getlimit($row=0)
        {
        if (isset($this->internal_limit[$row]))
            {
            return(count($this->internal_limit[$row]));
            }
        else
            {
            return(FALSE);
            }
        }

     //////////////////////////////////////////////////
     //Den Namen einer Datenreihe setzen
     function setrowname($name,$row=0)
        {
        $this->internal_rownames[$row]=$name;
        return(TRUE);
        }

    //////////////////////////////////////////////////
     //Größe des aktuellen Arrays holen
     function getsize($row=0)
        {
        if (isset($this->internal_data[$row]))
            {
            return(count($this->internal_data[$row]));
            }
        else
            {
            return(FALSE);
            }
        }
    
     //////////////////////////////////////////////////
     //Dateneingabe
     //Werden Arrays übergeben, so  werden diese als
     //entsprechende Datenpaare interpretiert
     //Mit Row kann eine Datenreihe Addressiert werden
     //Also füllt z.B. add(10,20,1) die Datenreihe 1
     //Die Reihenzählung beginnt bei Null
     function add($x,$y,$row=0)
        {
        //Immer ein Array erzwingen
        if (!is_array($x))
            {
            $x=array($x);
            }
        if (!is_array($y))
            {
            $y=array($y);
            }
            
        //Auf jeden Fall eine Integerzahl haben
        $row=intval($row);

        //Einfach alle Daten ins Array einspielen
        foreach ($x as $index => $xpos)
            {
            //Nur Werte mit einem korespondierenden Y zulassen
            if (isset($y[$index]))
                {
                $this->internal_data[$row][$xpos]=$y[$index];
                }
            }
        }

    //////////////////////////////////////////////////
    //Ein Datenpaar pushen
    //Die Funktionalität entspricht der Methode ADD
    //Allerdings werden Datenpaar mit einem Index höher
    //als $internal_limit hinten rausgepoppt
    function push($y,$row=0)
        {
        //Immer ein Array erzwingen
        if (!is_array($y))
            {
            $y=array($y);
            }
            
        //Auf jeden Fall eine Integerzahl haben
        $row=intval($row);
        
        //Wenn das Array nicht existiert, erzeugen wir es
        if (!isset($this->internal_data[$row]))
            {
            $this->internal_data[$row]=array_fill(0,$this->internal_limit,0);
            }
        
        //Array Differenzen bilden
        $diff=$this->internal_limit[$row] - count($y);
        if ($diff < 0) $diff=0;

        //Array verkürzen
        while (count ($this->internal_data[$row]) > $diff)
            {
            array_shift($this->internal_data[$row]);
            }

        //Und die Daten hinten drauflegen
        foreach ($y as $ypos)
            {
            array_push($this->internal_data[$row],$ypos);
            }
        }

    //////////////////////////////////////////////////
    // Die Grafik erzeugen
    // hier wird die Grafik erzeugt und die Funktion "process" aufgerufen,
    // wenn sie existiert
    function execute()
        {
        $result=FALSE;
        
        //Grenzen bestimmen
        $this->_setminmax();
        
        //Grafik initialisieren
        $this->_init();
        
        //Erzeugung OK ?
        if ($this->internal_handle!=FALSE)
            {
            $this->_initcolor();
            
            
            //Hintergrund füllen
            if ($this->background==FALSE)
                {
                imagefilledrectangle($this->internal_handle,0,0,$this->width,$this->height,$this->internal_color["back"]);
                }

            //Gibt es eine registrierte ProcessFunktion
            if ($this->internal_processor!=FALSE)
                {
                call_user_func(array(&$this,$this->internal_processor));
                }
                
            //Titel drüber
            if ($this->title!="")
                {
                imagestring($this->internal_handle,
                            $this->font + 1,
                            ($this->boxwidth - strlen($this->title) * $this->fontwidth) >> 1,
                            $this->boxtop - $this->fontheight,
                            $this->title,
                            $this->internal_color["text"]);
                }
            }
        return($result);
        }

        
    //////////////////////////////////////////////////
    // Grafik abspeichern
    function save($filename=FALSE,$mode="file")
        {
        //Wenn kein Dateiname gegeben wurde, speichern wir die Datei einfach so
        //ab
        if ($filename===FALSE)
            {
            $filename=$this->path.$this->_getimagename();
            }

        //Modeswitcher
        switch ($mode)
            {
            case ("web")    : header("Content-Type: image/png",TRUE);
                              header("Content-Transfer-Encoding: binary");
                              imagepng($this->internal_handle);
                              echo "honk";
                              break;
            case ("file")   : $result=@imagepng($this->internal_handle,$filename);      break;
            default         : $result=@imagepng($this->internal_handle,$filename);      break;
            }
            
        return($result);
        }


    //////////////////////////////////////////////////
    //Prozessor registrieren
    function registerprocessor($function)
        {
        $this->internal_processor=$function;
        }
        
    //////////////////////////////////////////////////
    //Hilfsfunktionen
    //////////////////////////////////////////////////

    //////////////////////////////////////////////////
    //Den Namen der Grafik
    function _getimagename()
        {
        //Wenn kein Name gesetzt ist, diesen erzwingen
        if ($this->image===FALSE)
            {
            $this->image=MD5(time()+rand()).".png";
            }
        return($this->image);
        }

    //////////////////////////////////////////////////
    //Min / Max werte setzen
    function _setminmax()
        {
        //Arrays nach X-Werten sortieren
        for ($row=0; $row < count($this->internal_data);$row++)
            {
            ksort($this->internal_data[$row]);
            }

        //Die Maximalwerte aus den Datenreihen holen
        $ymax=0;
        $ymin=0xffffffff;
        $xmax=0;
        $xmin=0xffffffff;
        foreach ($this->internal_data as $rows)
            {
            if (count($rows)>0)
                {
                //Y-Wert
                if (max($rows) > $ymax) $ymax=max($rows);
                if (min($rows) < $ymin) $ymin=min($rows);
                //X-Wert
                $keys=array_keys($rows);
                if (max($keys) > $xmax) $xmax=max($keys);
                if (min($keys) < $xmin) $xmin=min($keys);
                }
            }

        //Und global übernehmen
        $this->ymin=$ymin;
        $this->ymax=$ymax;
        $this->xmin=$xmin;
        $this->xmax=$xmax;
        
        //Werte-Spannweite
        $this->xspan=(0 + $this->xmax) - (0 + $this->xmin);
        $this->yspan=(0 + $this->ymax) - (0 + $this->ymin);

        //Spanweite normieren
        $this->boxleft  =$this->left;
        $this->boxright =$this->width  - $this->right;
        $this->boxtop   =$this->top;
        $this->boxbottom=$this->height - $this->bottom;
        
        $this->boxwidth =$this->boxright  - $this->boxleft;
        $this->boxheight=$this->boxbottom - $this->boxtop;
        }

    //////////////////////////////////////////////////
    //Die Grafik initiieren und den Handle setzen
    function _init()
        {
        //Backgorunddatei ziehen
        if (file_exists($this->background))
            {
            $this->internal_handle=imagecreatefrompng($this->background);

            //Resizen wenn die Grafik nicht der gewüschten Größe entspricht
            $this->_resize();
            }
        else
            {
            $this->background=FALSE;
            $this->internal_handle=imagecreatetruecolor($this->width,$this->height);
            }

        //Fontmetrics holen
        $this->fontheight=imagefontheight($this->font);
        $this->fontwidth =imagefontwidth ($this->font);
        }
        
    //////////////////////////////////////////////////
    //Eine Farbe bestimmen
    function _getcolor($r,$g,$b,$a=0)
        {
        return(imagecolorallocatealpha($this->internal_handle,$r,$g,$b,$a));
        }

    //////////////////////////////////////////////////
    //Farben für die Datenreihen erzeugen
    //Das Farbarray wird für jede Reihe mit einem Wert gefüllt und
    //zusätzlich die Indizees "text" "grid" "legend" "border" "back" gesetzt
    function _initcolor()
        {
        //Die Basisfarben
        $this->internal_color["back"]  =$this->_getcolor(255,255,255,  0);
        $this->internal_color["text"]  =$this->_getcolor(  0,  0,  0,  0);
        $this->internal_color["grid"]  =$this->_getcolor(128,128,128,  0);
        $this->internal_color["legend"]=$this->_getcolor(240,240,240, 64);
        $this->internal_color["border"]=$this->_getcolor(  0,  0,  0,  0);

        //Der Hintergrund wird transparent
        imagecolortransparent($this->internal_handle,$this->internal_color["back"]);
            
        //Und die Farbverläufe für die Datenreihen
        $diff=255 / count($this->internal_data);
        $r   =255;   $rd = $diff;
        $g   =128;   $gd = $diff;
        $b   =  0;   $bd = $diff;
            
        foreach ($this->internal_data as $index => $row)
             {
             $this->internal_color[$index]=$this->_getcolor($r,$g,$b,40);
                
             //Farbverlauf setzen
             $r+=$rd;
             $g+=$gd;
             $b+=$bd;

             if ($r>255) {$r=255; $rd=-$rd;};
             if ($g>255) {$g=255; $gd=-$gd;};
             if ($b>255) {$b=255; $bd=-$bd;};
            }
        }

    //////////////////////////////////////////////////
    //Die Legende einblenden
    function _paintlegend($xpos,$ypos)
        {
        //Wenn keine gezeichnet werden soll, brechen wir ab
        if (!$this->showlegend)
            {
            return(FALSE);
            }

        //Wieviele Zeilen stehen in der Legende ?
        $rows=count($this->internal_data);
            
        //Den längsten String holen
        $chars=0;
        foreach ($this->internal_rownames as $text)
            {
            if (strlen($text) > $chars)
                {
                $chars=strlen($text);
                }
            }

        //Wie hoch sind die ?
        $height=$rows * $this->fontheight + ($this->fontheight >> 1);
            
        //Wie breit
        $width =$chars * $this->fontwidth + 20;
        
        //Kasten
        imagefilledrectangle($this->internal_handle,$xpos,$ypos,$xpos + $width,$ypos + $height, $this->internal_color["legend"]);

        //Rahmen
        imagerectangle($this->internal_handle,$xpos,$ypos,$xpos + $width,$ypos + $height, $this->internal_color["border"]);

        //Farben und Texte
        $xpos+=3;
        $ypos+=$this->fontheight >> 2;
        foreach ($this->internal_rownames as $index => $entry)
            {
            //Box
            imagefilledrectangle($this->internal_handle,$xpos,$ypos +1,$xpos + 5,$ypos + $this->fontheight - 1 , $this->internal_color[$index]);
            imagerectangle($this->internal_handle,$xpos,$ypos +1,$xpos + 5,$ypos + $this->fontheight - 1 , $this->internal_color["border"]);

            //Text
            imagestring($this->internal_handle,$this->font,$xpos + 8,$ypos,$entry,$this->internal_color["text"]);
            
            //Positionen anpassen
            $ypos += $this->fontheight;
            }
        return(TRUE);
        }
        
    //////////////////////////////////////////////////
    //Hintergrundgrafik resizen
    function _resize()
        {
        $result=FALSE;

        if ( (imagesx($this->internal_handle)!=$this->width) &&
             (imagesy($this->internal_handle)!=$this->height) )
             {
            $image_new=imagecreatetruecolor($this->width,$this->height);
            //Resampling
            if (imagecopyresampled($image_new,$this->internal_handle,0,0,0,0,$this->width,$this->height,imagesx($this->internal_handle),imagesy($this->internal_handle)))
                {
                //Altes Bitmap freigeben
                imagedestroy($this->internal_handle);

                //Neues setzen
                $this->internal_handle=$image_new;
            
                $result=TRUE;
                }
            }
        return($result);
        }

    //////////////////////////////////////////////////
    // Der Grafikprozessor
    function _processor()
        {
        //Reine Dummyfunktion
        }
    }
    
    
////////////////////////////////////////////////////////////////////////////////////
///
/// Klasse für Kuchen
///
////////////////////////////////////////////////////////////////////////////////////
class piechart extends chartbase
    {
    //Soll an die Strücke Text geschrieben werden
    var $showentries = TRUE;
    var $showtext    = FALSE;
    var $showpercent = TRUE;

    ///////////////////////////////////////////////////////////////////////////////
    //Den eigentlichen Grafikprozessor überdecken
    function _processor()
        {
        //Den Kuchen reinmalen
        $this->dopie();

        //Legende rein
        $this->_paintlegend(5,5,5);
        }

    /////////////////////////////////////////////////////////////////////////////////
    //Den Kuchen malen
    //Die Summe aus jeder Reihe bildet ein Stück
    function dopie()
        {
        //Summen bilden
        $data=array();
        foreach ($this->internal_data as $index => $row)
            {
            $data[$index]=array_sum($row);
            }
        $sum=array_sum($data);
        if ($sum <= 0) $sum=1;

        //Und nun die Teile einzeichenen
        $angle=0;
        $x=($this->width >> 1);
        $y=($this->height>> 1);

        //Wenn Text eingeblendet werden soll, brauchen wir mehr Platz
        if ( $this->showentries )
            {
            $h=$this->boxheight * 0.82;
            $radius=$h / 1.9;
            }
        else
            {
            $h=$this->boxheight * 0.98;
            }
        $w=$h;

        foreach ($data as $index => $part)
            {
            //Endwinkel bestimmen
            $end=$angle + ($part / $sum * 361);
            if ($end >360) $end=360;

            //Spalte einzeichnen
            imagefilledarc($this->internal_handle,$x,$y,$w,$h,$angle,$end,$this->internal_color[$index]  ,IMG_ARC_PIE);
            //Rand drumrum
            imagefilledarc($this->internal_handle,$x,$y,$w,$h,$angle,$end,$this->internal_color["border"]  ,IMG_ARC_EDGED | IMG_ARC_NOFILL);

            //Text einblenden
            if ($this->showentries)
                {
                $text="";
                if ($this->showtext)
                    {
                    $text.=$this->internal_rownames[$index];
                    }
                if ($this->showpercent)
                    {
                    $text.=" ".number_format($part/$sum * 100,1)."%";
                    }

                //Polar in Kartisisch
                $rad=deg2rad( ($angle + $end) >>1 );
                $xpos=cos($rad) * $radius;
                $ypos=sin($rad) * $radius;

                //Textgrößen berücksichtigen
                if ($xpos < 0)
                    {
                    $xpos-=(strlen($text) * $this->fontwidth);
                    }
                if ($ypos < 0)
                    {
                    $ypos-=$this->fontheight;
                    }
                $xpos+=$x;
                $ypos+=$y;

                //Text einblenden
                imagestring($this->internal_handle,$this->font,$xpos,$ypos,$text,$this->internal_color["text"]);
                }

            $angle=$end;
            }
        //Und noch ein Rand drumrum
        imageellipse($this->internal_handle,$x,$y,$w,$h,$this->internal_color["border"]);
        }
    }
</script>