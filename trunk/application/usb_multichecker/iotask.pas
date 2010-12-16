unit iotask;

interface
uses
  unit_typedefs,Classes,class_diskio,class_checksum;
  
type
  PCRC32Thread = ^TCRC32Thread;
  TCRC32Thread = class(TThread)
    private
    protected
      procedure Execute; override;
    public
      state  : string;
      crc    : string;
      device : TDiskIO;
      busy   : boolean;
end;
implementation

procedure TCRC32Thread.Execute;
var
  Buffer   : array of Byte;
  u32Read  : unsigned32;
  u32Burst : unsigned32;
  u64Sector: unsigned32;
  MD5      : TMD5;
begin
  self.crc:='NAN';
  if (assigned(self.device)) then
    begin
      with self.device do
        begin
          u32Burst := 1;

          MD5:=TMD5.create();
          MD5.init();

          //Buffer Setzen
          SetLength(Buffer, (SectorSize * u32Burst) );

          //Und los
          u64Sector:=0;
          while (u64Sector < SectorCount) and (not Self.Terminated) do
             begin
               if (Read(u64Sector,Addr(Buffer[0]),u32Burst,u32Read)=TRUE) then
                 begin
                   //Prüfsumme bauen
                   MD5.add(Addr(Buffer[0]),u32Read * SectorSize);

                   //Um die gelesenen Sekotoren vorschieben
                   inc(u64Sector,u32Read);
                 end
               else
                 begin
                   inc(u64Sector,1);
                 end;
             end;

          self.crc:=MD5.Finalize().sChecksum;
          MD5.Free();
        end;
    end;
  //Speicher freigeben
  SetLength(Buffer,0);
end;

end.

