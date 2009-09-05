<script language="PHP">
//////////////////////////////////////////////////////////////////////////
//Direkt beim Laden instanzieren
//Quasi als Singleton, was unter PHP4 nur extrem unelegant zu handhaben ist
if (!isset($GLOBALS["profiler"])) $GLOBALS["profiler"]=new profiler();

//////////////////////////////////////////////////////////////////////////
/// Die eigentliche Klasse
class profiler
    {
    var $data      = array();
    var $time      = 0;
    
    var $delimiter = ";";

    //////////////////////////////////////////////////////////////////////////
    //Konstruktor    
    function profiler()
      {
      declare(ticks = 1);
      register_shutdown_function(array($this,"destroy"));
      register_tick_function(array($this,"_tracker"));
      $this->time=microtime(TRUE);      
      }  
    
    //////////////////////////////////////////////////////////////////////////
    //Destruktor  
    function destroy()
      {
      unregister_tick_function(array($this,"_tracker"));
      $this->analyze();
      }

    //////////////////////////////////////////////////////////////////////////
    //Analyse der aufgezeichneten Daten
    function analyze()
      {
      $result=array();
      
      //Trace eindampfen
      $runtime=0;
      foreach ($this->data as $function => $value)
        {
        $result[$function]["name"]           = $function;
        $result[$function]["calls"]          = count($value["runtime"]);
        $result[$function]["total runtime"]  = array_sum($value["runtime"]);
        $result[$function]["min runtime"]    = min($value["runtime"]);
        $result[$function]["max runtime"]    = max($value["runtime"]);
        
        $runtime += $result[$function]["total runtime"];
        }
      
      //Statistik bilden
      foreach ($result as $function => $value)
        {
        $result[$function]["percent"] = round( ( ( $result[$function]["total runtime"] / $runtime ) * 100),2);
        }
        
      ksort($result);

      //Und als CSV abspeichern
      $output="class::function,num_calls,total_runtime,min_runtime,max_runtime,percent_time\r\n";
      $output=str_replace(",",$this->delimiter,$output);
      
      
      foreach ($result as $function => $value)
        {
        $output.=implode($this->delimiter,$value)."\r\n";
        }
      
      //German Style
      $output=str_replace(".",",",$output);
      
      file_put_contents(PATH_TEMP."profile_".NOW.".csv",$output);
      unset($result);
      }
      
    //////////////////////////////////////////////////////////////////////////
    //Tracer
    function _tracker()
      {
      $trace = debug_backtrace();
      $runtime = (microtime(true) - $this->time) * 1000;
      
  
      //Fehlende Wert nachpflegen
      $trace[1]["class"]=isset($trace[1]["class"])?$trace[1]["class"]:"none";
      $trace[2]["class"]=isset($trace[2]["class"])?$trace[2]["class"]:"none";
  
      $trace[1]["file"]=isset($trace[1]["file"])?$trace[1]["file"]:"same";
      $trace[2]["file"]=isset($trace[2]["file"])?$trace[2]["file"]:"same";

      $trace[1]["function"]=isset($trace[1]["function"])?$trace[1]["function"]:"unknown";
      $trace[2]["function"]=isset($trace[2]["function"])?$trace[2]["function"]:"unknown";

      $trace[1]["line"]=isset($trace[1]["line"])?$trace[1]["line"]:"unknown";
      $trace[2]["line"]=isset($trace[2]["line"])?$trace[2]["line"]:"unknown";

      //Nur fremde Traces aufzeichnen
      if ( $trace[1]["class"] != "profiler" )
        {

        $index=$trace[1]["class"]."::".$trace[1]["function"];

        $this->data[$index]["time"][]     = microtime(TRUE);
        $this->data[$index]["runtime"][]  = $runtime;
        $this->data[$index]["memory"][]   = memory_get_usage(TRUE);
        $this->data[$index]["call"][]     = $trace[1]["file"]." (".$trace[1]["line"].") ".$trace[1]["class"]."::".$trace[1]["function"];
        $this->data[$index]["calledby"][] = $trace[2]["file"]." (".$trace[2]["line"].") ".$trace[2]["class"]."::".$trace[2]["function"];
        
//        $this->data[$index][$number]["trace"]   =$trace;
        }
      
      $this->time=microtime(TRUE);      
      }
    }

</script>