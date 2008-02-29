unit Unit_UserConfig;
{
 ***************************************************************************
 *                                                                         *
 *   This source is free software; you can redistribute it and/or modify   *
 *   it under the terms of the GNU General Public License as published by  *
 *   the Free Software Foundation; either version 2 of the License, or     *
 *   (at your option) any later version.                                   *
 *                                                                         *
 *   This code is distributed in the hope that it will be useful, but      *
 *   WITHOUT ANY WARRANTY; without even the implied warranty of            *
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *
 *   General Public License for more details.                              *
 *                                                                         *
 *   A copy of the GNU General Public License is available on the World    *
 *   Wide Web at <http://www.gnu.org/copyleft/gpl.html>. You can also      *
 *   obtain it by writing to the Free Software Foundation,                 *
 *   Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.        *
 *                                                                         *
 ***************************************************************************
Author: Sven Lorenz / Borg@Sven-of-Nine.de
}

////////////////////////////////////////////////////////////////////////////////
///
/// Unit zur vereinfachten Speicherung von User-Daten
///
/// Mittels Save werden die Eigenschaften ALLER Komponenten in einem Formular
/// gespeichert.
///
/// Load holt diese dann wieder
///
///(c) 2005 Borg@Sven-of-Nine.de
///
///
///
////////////////////////////////////////////////////////////////////////////////
interface
uses Classes;
type
  TUserConfig = class(TObject)
  private
    { Private-Deklarationen }
    //Direkter Zugriff auf Eigenschaften
    //set properties using winapi
    function IsProperty  (Obj : TObject; sProp : String):Boolean;
    function SetProperty (Obj : TObject; sProp:String; vValue: variant):Boolean;
    function HasAncestor (Child:TComponent; Name:String):Boolean;

    function ClearMemory ():Boolean;
    function SetMemory   (MaxMemory:Integer):Boolean;

  public
    { Public-Deklarationen }
    constructor Create (MaxMemory:Integer=10);
    destructor  Destroy;

    //Komponenten in Datei schreiben
    //save/load components to/from file
    function SaveToFile  (Component:TComponent;sFilename:String):boolean;
    function LoadFromFile(Component:TComponent;sFilename:String):boolean;

    //Komponenten in Speicher schreiben (UNDO-Funktion)
    //save/load components to/from mem
    function SaveToMemory(Component:TComponent;Index:integer):boolean;
    function LoadFromMemory(Component:TComponent;Index:integer):boolean;

    function SaveMemoryToFile   (sFilename:String):Boolean;
    function LoadMemoryFromFile (sFilename:String):Boolean;
  end;

implementation
uses SysUtils,Controls,Forms,TypInfo;

var
   aMemStream : Array of TMemoryStream;
////////////////////////////////////////////////////////////////////////////////
/// Konstruktor und Destruktor
////////////////////////////////////////////////////////////////////////////////
constructor TUserConfig.Create (MaxMemory:Integer=10);
begin
     SetMemory(MaxMemory);
end;


destructor  TUserConfig.Destroy();
begin
     ClearMemory();
end;

////////////////////////////////////////////////////////////////////////////////
/// Prüfen, ob ein Object die gewünschte Eigenschaft hat
/// Check for properties
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.IsProperty(Obj : TObject; sProp : String):Boolean;
var
   plList  : tPropList;
   iIndex1 : integer;
   iIndex2 : integer;
begin
      result := FALSE;
      //Alle verfügbaren Properties holen
      //get properties
      iIndex2:= GetPropList( PTypeInfo(Obj.ClassInfo),
                            [ tkUnknown,tkVariant,tkInteger,tkInt64,tkFloat,
                              tkString,tkWString,tkLString,tkChar,tkWChar,
                              tkEnumeration,tkSet,tkClass,tkMethod,tkArray,
                              tkDynArray,tkRecord, tkInterface], @plList);
      //nach der gewünschten suchen
      //search for the wanted
      iIndex1:=0;
      while (iIndex1 < iIndex2) do
          begin
               if plList[iIndex1].Name = sProp then
                  begin
                       result := TRUE;
                       iIndex1:=iIndex2;
                  end;
               inc(iIndex1);
          end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Eine Egenschaft direkt setzen
/// set properties
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.SetProperty (Obj : TObject; sProp:String; vValue: variant):Boolean;
begin
     if (IsProperty(Obj, sProp)) then
        begin
             SetPropValue(Obj, sProp, vValue);
             result := TRUE;
        end
     else
        begin
             result := false;
        end;
end;

////////////////////////////////////////////////////////////////////////////////
/// Nach einem Vorfahr mit dem Namen "Name" suchen
/// check for ancestor named "Name"
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.HasAncestor(Child:TComponent; Name:String):Boolean;
var
   cWork : TComponent;
begin
     Result:=FALSE;
     cWork:=Child;
     while (cWork.HasParent) do
           begin
                //Eltern holen
                cWork:=cWork.GetParentComponent;
                //Sind die Eltern die gesuchten ?
                if (cWork.Name=Name) then
                   begin
                        //Dann Suche beenden
                        Result:=TRUE;
                        break;
                   end;
           end;
     cWork:=nil;
end;

function TUserConfig.SetMemory   (MaxMemory:Integer):Boolean;
var
   iIndex : integer;
begin
     //Alle angeforderten Speicherstreams initialisieren
     //initialize memorystreams
     if (MaxMemory>255) then MaxMemory:=255;
     try
        SetLength(aMemStream,MaxMemory);
        for iIndex:=0 to MaxMemory -1 do
            begin
                 aMemStream[iIndex]:=TMemoryStream.Create;
            end;
        Result:=TRUE;
     except
           Result:=FALSE;
     end;
end;


function TUserConfig.ClearMemory ():Boolean;
var
   iIndex : integer;
begin
     //Alle angeforderten Speicherstreams freimachen
     //free all
     iIndex:=0;
     while (iIndex < Length(aMemStream)) do
         begin
              aMemStream[iIndex].Free;
              inc(iIndex);
         end;
     SetLength(aMemStream,0);
     Result:=TRUE;
end;


////////////////////////////////////////////////////////////////////////////////
/// Save all components to disk
/// alle komponenten in datei speichern
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.SaveToFile  (Component:TComponent;sFilename:String):boolean;
var
   hFile  : Integer;
   Stream : THandleStream;
   iIndex : integer;
   sName  : String[255];
   cWork  : TComponent;
begin
     Result:=FALSE;

     //Datei auf jeden Fall immer neu erzeugen
     //Create File
     hFile:=FileCreate(sFilename);
     if (hFile>0) then
        begin
             //Die Hauptkomponente finden (das Formular)
             //Find parent
             cWork:=Component;
             while (cWork.HasParent) do
                   begin
                        cWork:=cWork.GetParentComponent;
                   end;
             //Stream erzeugen
             //Create stream
             Stream:=THandleStream.Create(hFile);
             try
                //Und los
                //enumerate all
                for iIndex:=0 to cWork.ComponentCount-1 do
                    begin
                         //Ist es ein Win-Control und eine Nachfahre der gewünschten Componente?
                         //save only TWinControls and childs of Component
                         if (cWork.Components[iIndex] is TWinControl) and
                            (HasAncestor(cWork.Components[iIndex],Component.Name)) then
                            begin
                                 //Hier ein paar Ausnahmen
                                 //some exceptions
                                 if (cWork.Components[iIndex].ClassName<>'TFlatTitlebar') and
                                    (cWork.Components[iIndex].ClassName<>'TFlatSpinEd1itInteger')
                                    then
                                 begin
                                      //Erst den Namen
                                      //save name first
                                      sName := cWork.Components[iIndex].Name;
                                      Stream.Write(sName,Length(sName)+1);

                                      //Und dann die Komponente hinterher
                                      //and component
                                      Stream.WriteComponent(cWork.Components[iIndex]);
                                 end;
                            end;
                    end;
                Result:=TRUE;
             finally
                //Fertig
                //done
                Stream.Free;
             end;
             //close handle
             FileClose(hFile);
        end;
     cWork:=nil;
end;


////////////////////////////////////////////////////////////////////////////////
/// load all components from disk
/// alle komponenten aus datei laden
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.LoadFromFile(Component:TComponent;sFilename:String):boolean;
var
   hFile  : Integer;
   Stream : THandleStream;
   iIndex : integer;
   sName  : String[255];
   iName  : integer;
   cWork  : TComponent;
begin
     Result:=FALSE;
     //Date öffnen
     //open read
     hFile:=FileOpen(sFilename,fmOPENREAD);
     if (hFile>0) then
        begin
             //Das die Hauptkomponente finden (das Formular)
             cWork:=Component;
             while (cWork.HasParent) do
                   begin
                        cWork:=cWork.GetParentComponent;
                   end;

             //Stream erzeugen
             //create stream
             Stream:=THandleStream.Create(hFile);
             try
                //Vorne anfangen
                //from the beginning
                Stream.Position:=0;
                //Und kpl. durchwurstem
                //the whole file
                while (Stream.Position < Stream.Size) do
                      begin
                           //erstes byte des namens
                           //first byte of Name
                           Stream.Read(sName[0],1);
                           //Größe rausholen
                           //get size
                           iName:=Byte(sName[0]);
                           //Und den ganzen Namen lesen
                           //Read the whole name
                           Stream.Read(sName[1],iName);

                           //Object holen
                           //get object
                           try
                              //Nach dem namen suchens
                              //search for the name
                              for iIndex:=0 to cWork.ComponentCount-1 do
                                  begin
                                       if (cWork.Components[iIndex].Name=sName) then
                                          begin
                                               //Bei allem, was Checked hat, dies erst auf FALSE
                                               // setzen
                                               //Uncheck all "checkables"
                                               SetProperty(cWork.Components[iIndex],'Checked',FALSE);

                                               //Und dann erst laden
                                               //load
                                               Stream.ReadComponent(cWork.Components[iIndex]);
                                          end;
                                  end;
                           except
                           end;
                      end;
             finally
                //done
                Stream.Free;
             end;
             FileClose(hFile);
        end;
     cWork:=nil;
end;

////////////////////////////////////////////////////////////////////////////////
/// Save all components to memory
/// alle komponenten in speicher schreiben
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.SaveToMemory  (Component:TComponent;Index:integer):boolean;
var
   iIndex : integer;
   sName  : String[255];
   cWork  : TComponent;
begin
     Result:=FALSE;
     if (Index < 0) or (Index >= Length(aMemStream)) then Exit;
     try
        //Die Hauptkomponente finden (das Formular)
        cWork:=Component;
        while (cWork.HasParent) do
              begin
                   cWork:=cWork.GetParentComponent;
              end;

        //Den Stream leermachen
        aMemStream[Index].SetSize(0);
        for iIndex:=0 to cWork.ComponentCount-1 do
            begin
                 if (cWork.Components[iIndex] is TWinControl) and
                    (HasAncestor(cWork.Components[iIndex],Component.Name)) then
                    begin
                         if (cWork.Components[iIndex].ClassName<>'TFlatTitlebar') and
                            (cWork.Components[iIndex].ClassName<>'TFlatSpinEd1itInteger')
                            then
                                begin
                                     sName := Component.Components[iIndex].Name;
                                     aMemStream[Index].Write(sName,Length(sName)+1);
                                     aMemStream[Index].WriteComponent(cWork.Components[iIndex]);
                                end;
                    end;
            end;
        Result:=TRUE;
     finally
          cWork:=nil;
     end;
end;

////////////////////////////////////////////////////////////////////////////////
/// load components[index] from memory
/// komponenten[index] aus speicher lesen
////////////////////////////////////////////////////////////////////////////////
function TUserConfig.LoadFromMemory(Component:TComponent;Index:Integer):boolean;
var
   iIndex : integer;
   sName  : String[255];
   iName  : integer;
   cWork  : TComponent;
begin
     result:=FALSE;
     if (Index < 0) or (Index >= Length(aMemStream)) then Exit;
     try
        cWork:=Component;
        while (cWork.HasParent) do
              begin
                   cWork:=cWork.GetParentComponent;
              end;

        aMemStream[Index].Position:=0;
        while (aMemStream[Index].Position < aMemStream[Index].Size) do
              begin
                   aMemStream[Index].Read(sName[0],1);
                   iName:=Byte(sName[0]);
                   aMemStream[Index].Read(sName[1],iName);
                   try
                      for iIndex:=0 to cWork.ComponentCount-1 do
                          begin
                               if (cWork.Components[iIndex].Name=sName) then
                                  begin
                                       SetProperty(cWork.Components[iIndex],'Checked',FALSE);
                                       aMemStream[Index].ReadComponent(cWork.Components[iIndex]);
                                  end;
                          end;
                   except
                   end;
              end;
        result:=TRUE;
     finally
            cWork:=nil;
     end;
end;

////////////////////////////////////////////////////////////////////////////////
function TUserConfig.SaveMemoryToFile   (sFilename:String):Boolean;
var
   hFile  : Integer;
   Stream : THandleStream;
   iIndex : integer;
   iSize  : integer;
   cCount : Cardinal;
   bTemp  : Byte;
begin
     Result:=FALSE;

     //Datei auf jeden Fall immer neu erzeugen
     //Create File
     hFile:=FileCreate(sFilename);
     if (hFile>0) then
        begin
             //Stream erzeugen
             //Create stream
             FileSeek(hFile,0,0);
             Stream:=THandleStream.Create(hFile);
             try
                iSize:=Length(aMemStream);
                Stream.Position:=0;

                //Anzahl der Einträge speichern
                bTemp:=Byte(iSize);
                Stream.Write(bTemp,1);

                //Und die MemStreams hinterher
                for iIndex:=0 to iSize-1 do
                    begin
                         //Size
                         cCount:=aMemStream[iIndex].Size;

                         //Größe eines Eintrages speichern
                         bTemp:=Byte(cCount shr 24); Stream.Write(bTemp,1);
                         bTemp:=Byte(cCount shr 16); Stream.Write(bTemp,1);
                         bTemp:=Byte(cCount shr  8); Stream.Write(bTemp,1);
                         bTemp:=Byte(cCount shr  0); Stream.Write(bTemp,1);

                         //Und den Eintrag hinterher
                         aMemStream[iIndex].SaveToStream(Stream);
                    end;
                Result:=TRUE;
             finally
                //Fertig
                //done
                Stream.Free;
             end;
             //close handle
             FileClose(hFile);
        end;
end;

////////////////////////////////////////////////////////////////////////////////
function TUserConfig.LoadMemoryFromFile (sFilename:String):Boolean;
var
   hFile    : Integer;
   Stream   : THandleStream;
   iIndex   : integer;
   iMemSize : integer;
   iSize    : Integer;
   cCount   : Cardinal;
   bCount   : byte;
   sMem     : TMemoryStream;
begin
     Result:=FALSE;
     //Date öffnen
     //open read
     hFile:=FileOpen(sFilename,fmOPENREAD);
     if (hFile>0) then
        begin
             //Stream erzeugen
             //create stream
             Stream:=THandleStream.Create(hFile);
             try
                //Vorne anfangen
                //from the beginning
                Stream.Position:=0;

                //Alle Streams löschen
                ClearMemory();

                //Anzahl der Streams holen
                Stream.Read(bCount,1);
                iSize:=bCount;

                for iIndex:=0 to iSize -1 do
                    begin
                         //Größe des Stream holen
                         Stream.Read(bCount,1); cCount:=         bCount shl 24;
                         Stream.Read(bCount,1); cCount:=cCount + bCount shl 16;
                         Stream.Read(bCount,1); cCount:=cCount + bCount shl  8;
                         Stream.Read(bCount,1); cCount:=cCount + bCount shl  0;

                         //Speicherarray anpassen
                         iMemSize:=Length(aMemStream);
                         SetLength(aMemStream,iMemSize+1);
                         aMemStream[iMemSize]:=TMemoryStream.Create;

                         //Größe setzen
                         aMemStream[iMemSize].SetSize(cCount);

                         //Und lesen
                         aMemStream[iMemSize].CopyFrom(Stream,cCount);
                    end;
             finally
                //done
                Stream.Free;
             end;
             FileClose(hFile);
        end;
end;


end.
