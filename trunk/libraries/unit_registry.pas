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

unit unit_registry;
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
/// Easy-Registry 0.1 (c) 2004 Borg@Sven-of-Nine.de
///
/// Simplyfied Registry-Access using variant
///////////////////////////////////////////////////////////////////////////////////////////////
///
///////////////////////////////////////////////////////////////////////////////////////////////
/// HowTo
///////////////////////////////////////////////////////////////////////////////////////////////
///Main Funktions
/// Read
/// Variable:=RegKeyRead(HKEY_...,'RegPath',DefaultValue);
/// Variable can be Integer,String,Boolen,DateTime;
/// if Value doesn't exists DefaultValue will be returned
///
/// Write
/// RegKeyRead(HKEY_...,'RegPath',Value);
/// Value can be Integer,String,Boolen,DateTime;
///
///Return all Values
///function RegEnumVals(HK:HKEY;Pfad:String):TStringList;
///////////////////////////////////////////////////////////////////////////////////////////////
///Additional Funktions
///RegKeyExists
/// RegKeyExists(HK:HKEY;Path:String):Boolean;
///
///Delete
/// function RegKeyDelete(HK:HKEY;Path:String):Boolean;
/// function RegValDelete(HK:HKEY;Path:String):Boolean;
///
///Save and Load Component Settings from your form
/// function RegSaveComponent(HK:HKEY;Path:String;MyForm:TComponent):Boolean;
/// function RegLoadComponent(HK:HKEY;Path:String;MyForm:TComponent):Boolean;
///
///////////////////////////////////////////////////////////////////////////////////////////////
///SOME EXAMPLES
///
/// Read a String from Registry
/// RegKeyRead(HKEY_CURRENT_USER,'Software\MySoftware\MyString,'Not found');
/// Returns the value of MyString or 'Not found' if mystring doesn't exist or is no string
///
/// Save a Date to Registry
/// RegKeySave(HKEY_CURRENT_USER,'Software\MySoftware\ToDay',Now());
/// save the current TimeDate to Registry
///
///Remove a registry-value
///
/// RegValDelete(HKEY_CURRENT_USER,'Software\MySoftware\NoMoreNeeded');
/// Removes the Value called NoMoreNeeded
///
/// RegKeyDelete(HKEY_CURRENT_USER,'Software\MySoftware\NoMoreNeeded');
/// Removes the Key called NoMoreNeeded (and ALL subkeys and values)
///
///////////////////////////////////////////////////////////////////////////////////////////////
interface

uses Registry,SysUtils,Windows,Classes,Variants;

//test if a key exists or not
function RegValExists(HK:HKEY;Pfad:String):Boolean;

//Reads from Registry (string/integer/boolean/datetime)
function RegValRead (HK:HKEY;Pfad:String;Default:Variant):Variant;
//Saves to Registry (string/integer/boolean/datetime)
function RegValWrite(HK:HKEY;Pfad:String;Value:Variant):Boolean;

//Saves Component-Values to Registry
function RegSaveComponent(HK:HKEY;Pfad:String;Comp:TComponent):Boolean;
function RegLoadComponent(HK:HKEY;Pfad:String;Comp:TComponent):Boolean;

//Removes a Reg-Value / Reg-Key
function RegKeyDelete(HK:HKEY;Pfad:String):Boolean;
function RegValDelete(HK:HKEY;Pfad:String):Boolean;

//Returns all ValuesNames
function RegEnumValueNames (HK:HKEY;Pfad:String):TStringList;



///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
///////////////////////////////////////////////////////////////////////////////////////////////
implementation
uses
    stdctrls,Forms;

const
     UNIXStartDate : TDateTime=255690.0;

///////////////////////////////////////////////////////////////////////////////////////////////
//Hilfsfunktionen um Datum in der Registry zu speichern
///////////////////////////////////////////////////////////////////////////////////////////////
function DateTimeToTimeStamp(date:TDateTime):LongInt;
begin
//     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=Round(( date - UNIXStartDate)*86400);
end;

function TimeStampToDateTime(timestamp:LongInt):TDateTime;
begin
//     UNIXStartDate:=EncodeDate(1970,1,1)+EncodeTime(2,0,0,0);
     Result:=(timestamp / 86400)+UNIXStartDate;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
function RegValExists(HK:HKEY;Pfad:String):Boolean;
var
   Reg :TRegistry;
begin
     Result:=FALSE;
     Reg:=TRegistry.Create;
     try
        Reg.RootKey:=HK;
        if (Reg.OpenKey(ExtractFilePath(Pfad),FALSE)) then
           begin
                Result:=Reg.ValueExists(ExtractFileName(Pfad));
           end;
     finally
     end;
     Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Einen Registrierstring lesen und bei einem Fehler den Default-Wert zurückgeben
function RegValRead(HK:HKEY;Pfad:String;Default:Variant):Variant;
var
   Reg :TRegistry;
begin
     //Wenn der Key nicht existiert, dann den Defaultwert zurückgeben
     if (RegValExists(HK,Pfad)=FALSE) then
        begin
             Result:=Default;
             Exit;
        end;

     Reg:=TRegistry.Create;
     try

        Reg.RootKey:=HK;
        Reg.OpenKey(ExtractFilePath(Pfad),FALSE);

        //Und jetzt den entsprechenden Datentyp wählen
        case VarType(Default) of
             varInteger : try Result:=Reg.ReadInteger (ExtractFileName(Pfad)); except Result:=Default; end;
             varBoolean : try Result:=Reg.ReadBool    (ExtractFileName(Pfad)); except Result:=Default; end;
             varString  : try Result:=Reg.ReadString  (ExtractFileName(Pfad)); except Result:=Default; end;
             varDate    : try Result:=TimeStampToDateTime(Reg.ReadInteger (ExtractFileName(Pfad))); except Result:=Default; end;
        end;

     except
     end;
     Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Einen Registrierstring lesen und bei einem Fehler den Default-Wert zurückgeben
function RegValWrite(HK:HKEY;Pfad:String;Value:Variant):Boolean;
var
   Reg :TRegistry;
begin
     Result:=TRUE;
     Reg:=TRegistry.Create;
     try
        Reg.RootKey:=HK;
        Reg.OpenKey(ExtractFilePath(Pfad),TRUE);

        //Und jetzt den entsprechenden Datentyp wählen und in die Reg schreiben
        case VarType(Value) of
             varInteger : try Reg.WriteInteger (ExtractFileName(Pfad),Value); except Result:=FALSE; end;
             varBoolean : try Reg.WriteBool    (ExtractFileName(Pfad),Value); except Result:=FALSE; end;
             varString  : try Reg.WriteString  (ExtractFileName(Pfad),Value); except Result:=FALSE; end;
             varDate    : try Reg.WriteInteger (ExtractFileName(Pfad),DateTimeToTimeStamp(Value)); except Result:=FALSE; end;
        end;

     except
     end;
     Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Einen RegistrierWert löschen
function RegValDelete(HK:HKEY;Pfad:String):Boolean;
var
   Reg :TRegistry;
begin
        Reg:=TRegistry.Create;
     try
        Reg.RootKey:=HK;
        Reg.OpenKey(ExtractFilePath(Pfad),TRUE);
        Reg.DeleteValue(ExtractFileName(Pfad));
        Result:=Not RegValExists(HK,Pfad);
     finally
     end;
        Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Einen Registrierkey löschen
function RegKeyDelete(HK:HKEY;Pfad:String):Boolean;
var
   Reg :TRegistry;
begin
        Reg:=TRegistry.Create;
     try
        Reg.RootKey:=HK;
        Reg.OpenKey(ExtractFilePath(Pfad),TRUE);
        Reg.DeleteKey(ExtractFileName(Pfad));
        Result:=Not RegValExists(HK,Pfad);
     finally
     end;
        Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Alle ValueNamen eines Pfades ausgeben
function RegEnumValueNames(HK:HKEY;Pfad:String):TStringList;
var
   Reg :TRegistry;
begin
        Reg:=TRegistry.Create;
        Result:=TStringList.Create;
        Result.Clear;
     try
        Reg.RootKey:=HK;
        Reg.OpenKey(Pfad,FALSE);
        //Alle Values holen
        Reg.GetValueNames(Result);
     finally
     end;
        Reg.Free;
end;

///////////////////////////////////////////////////////////////////////////////////////////////
//Speichert die wichtigsten Werte eine Componente (und deren Kinder) in die Registry
function RegSaveComponent(HK:HKEY;Pfad:String;Comp:TComponent):Boolean;
var
   z    : integer;
begin
     Result:=TRUE;
     //Wenn der mit einem Backslash aufhört, dann abbrechen
     if (Pfad[Length(Pfad)]='\') then Exit;

     //Typecasten
     Comp:=Comp as TForm;

     //Enumerieren
     z:=Comp.ComponentCount-1;

     //Alle Einträge erstmal entfernen
     RegKeyDelete(HKEY_CURRENT_USER,Pfad);

     //Formular sichern
     with Comp as TForm do
          begin
               RegValWrite(HKEY_CURRENT_USER,pfad+'\Top' ,Top);
               RegValWrite(HKEY_CURRENT_USER,pfad+'\Left',Left);
          end;

     //Nun Alle Komponenten durchnummerieren und deren Eigenschaften speichern
     try
     while (z>=0) do
           begin
                if (Comp.Components[z] is TButton) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TButton do
                             begin
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Enabled',Enabled);
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Caption',Caption);
                             end;
                   end;

                //CheckBox
                if (Comp.Components[z] is TCheckBox) then
                   begin
                        with Comp.Components[z] as TCheckBox do
                             begin
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Enabled',Enabled);
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Checked',Checked);
                             end;
                   end;

                //Radiobutton
                if (Comp.Components[z] is TRadioButton) then
                   begin
                        with Comp.Components[z] as TRadioButton do
                             begin
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Enabled',Enabled);
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Checked',Checked);
                             end;
                   end;

                //EditBox ?
                if (Comp.Components[z] is TEdit) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TEdit do
                             begin
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Enabled',Enabled);
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Text',Text);
                             end;
                   end;

                //ComboBox
                if (Comp.Components[z] is TComboBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TComboBox do
                             begin
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\Enabled',Enabled);
                                  RegValWrite(HKEY_CURRENT_USER,Pfad+Name+'\ItemIndex',ItemIndex);
                             end;
                   end;
                dec(z);
           end;
     except
           Result:=FALSE;
     end;
end;


///////////////////////////////////////////////////////////////////////////////////////////////
//Lädt die wichtigsten Werte eine Componente (und deren Kinder) in die Registry
function RegLoadComponent(HK:HKEY;Pfad:String;Comp:TComponent):Boolean;
var
   z    : integer;
begin
     Result:=TRUE;
     //Wenn der mit einem Backslash aufhört, dann abbrechen
     if (Pfad[Length(Pfad)]='\') then Exit;

     //Typecasten
     Comp:=Comp as TForm;

     //Enumerieren
     z:=Comp.ComponentCount-1;


     //Formular laden
     with Comp as TForm do
          begin
               Top :=RegValRead(HKEY_CURRENT_USER,pfad+'\Top' ,Top);
               Left:=RegValRead(HKEY_CURRENT_USER,pfad+'\Left',Left);
          end;

     //Nun Alle Komponenten durchnummerieren und deren Eigenschaften laden
     while (z>=0) do
           begin
                if (Comp.Components[z] is TButton) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TButton do
                             begin
                                  Enabled:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Enabled',True);
                             end;
                   end;

                if (Comp.Components[z] is TCheckBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TCheckBox do
                             begin
                                  Enabled:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Enabled',TRUE);
                                  Checked:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Checked',TRUE);
                             end;
                   end;

                if (Comp.Components[z] is TRadioButton) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TRadioButton do
                             begin
                                  Enabled:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Enabled',TRUE);
                                  Checked:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Checked',TRUE);
                             end;
                   end;

                if (Comp.Components[z] is TEdit) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TEdit do
                             begin
                                  Enabled:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Enabled',TRUE);
                                  Text:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Text','Text');
                             end;
                   end;

                if (Comp.Components[z] is TComboBox) then
                   begin
                        //Button ?
                        with Comp.Components[z] as TComboBox do
                             begin
                                  Enabled:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\Enabled',TRUE);
                                  ItemIndex:=RegValRead(HKEY_CURRENT_USER,pfad+Name+'\ItemIndex',-1);
                             end;
                   end;

                dec(z);
           end;
end;

end.
