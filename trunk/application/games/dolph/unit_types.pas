unit unit_types;
////////////////////////////////////////////////////////////////////////////////
/// Global Typen
////////////////////////////////////////////////////////////////////////////////

interface
uses unit_typedefs;

type TPosition = record
  XPos : signed32;
  YPos : signed32;
end;

type TAttributeValue = record
     Value    : unsigned32;
     MaxValue : unsigned32;
end;


function attributevalue(Value:unsigned32;MaxValue:unsigned32):TAttributeValue;
function attributevalueRND(rand:unsigned32;offset:unsigned32):TAttributeValue;

implementation

function attributevalue(Value:unsigned32;MaxValue:unsigned32):TAttributeValue;
begin
     result.Value   :=Value;
     result.MaxValue:=MaxValue;
end;

function attributevalueRND(rand:unsigned32;offset:unsigned32):TAttributeValue;
begin
     result.Value   :=random(rand) + offset;
     result.MaxValue:=result.Value;
end;

end.
