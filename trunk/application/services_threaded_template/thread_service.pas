unit thread_service;
/////////////////////////////////////////////////////////////////////////////////
///
/// (c) 2007 Borg@Sven-of-Nine.de / http://www.sven-of-nine.de
///
/// Hier l�uft der eigentlich Service ab
///
/// Alle Arbeiten des Services werden in der Methode Execute ausgef�hrt.
/// Um den Service steuerbar zu halten, MUSS Execute das Flag "terminated"
/// �berwachen darauf reagieren.
///
/////////////////////////////////////////////////////////////////////////////////

interface
uses classes,windows;

/////////////////////////////////////////////////////////////////////////////////
//Hier definieren wir uns einen Thread der einzig zur Ausf�hrung des
//services da ist.
type tsvcthread = class(tthread)
     protected
           procedure Execute(); override;
     public
           constructor Create();
end;

implementation

/////////////////////////////////////////////////////////////////////////////////
//Konstruktor
constructor tsvcthread.Create();
begin
     //unseren Thread immer suspended startem
     Inherited Create(TRUE);

     //Priorit�t sch�n niedrig setzen
     Self.Priority:=tpLowest;
end;

/////////////////////////////////////////////////////////////////////////////////
//Main
procedure tsvcthread.Execute();
begin
     while (self.Terminated = FALSE) do
           begin
                sleep(1000);
           end;
end;

end.
