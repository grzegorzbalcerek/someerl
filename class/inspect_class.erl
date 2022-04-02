%%%-------------------------------------------------------------------
%%% @author grzes
%%% @doc
%%% Module for parsing and inspecting JVM Class files.
%%% @end
%%%-------------------------------------------------------------------
-module(inspect_class).

%% API
-export([main/1]).

main([File]) ->
  io:format("Parsing ~s.~n", [File]),
  {ok, Bin} = file:read_file(File),
  inspectClassFile(Bin).

inspectClassFile(Bin) ->
  <<16#cafebabe:32,MinorVersion:16,MajorVersion:16,ConstantPoolCount:16,BinConstantPool/binary>> = Bin,
  io:format("MinorVersion.MajorVersion: ~w.~w.~n", [MinorVersion, MajorVersion]),
  io:format("ConstantPoolCount: ~w.~n", [ConstantPoolCount]),
  {BinClassFlags, CpDict} = inspectConstantPool(BinConstantPool, 1, ConstantPoolCount, dict:new()),
  <<ClassFlags:16, BinThisClass/binary>> = BinClassFlags,
  inspectClassFlags(ClassFlags),
  <<ThisClass:16,SuperClass:16,InterfacesCount:16,BinInterfaces/binary>> = BinThisClass,
  io:format("ThisClass: ~w [~s].~n", [ThisClass, decodeCpIndex(ThisClass, CpDict)]),
  io:format("SuperClass: ~w [~s].~n", [SuperClass, decodeCpIndex(SuperClass, CpDict)]),
  io:format("InterfacesCount: ~w.~n", [InterfacesCount]),
  BinFieldsCount = inspectInterfaces(BinInterfaces, 0, InterfacesCount, CpDict),
  <<FieldsCount:16,BinFields/binary>> = BinFieldsCount,
  io:format("FieldsCount: ~w.~n", [FieldsCount]),
  BinMethodsCount = inspectFields(BinFields, 0, FieldsCount, CpDict),
  <<MethodsCount:16,BinMethods/binary>> = BinMethodsCount,
  io:format("MethodsCount: ~w.~n", [MethodsCount]),
  BinAttributesCount = inspectMethods(BinMethods, 0, MethodsCount, CpDict),
  <<AttributesCount:16,Bin10/binary>> = BinAttributesCount,
  io:format("AttributesCount: ~w.~n", [AttributesCount]),
  Bin11 = inspectAttributes(Bin10, 0, AttributesCount, CpDict),
  io:format("Uninspected Bin (expected: <<>>): ~w.~n", [Bin11]).

inspectConstantPool(Bin, K, ConstantPoolCount, CpDict) when K =:= ConstantPoolCount -> {Bin, CpDict};
inspectConstantPool(<<1, Length:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  {Text, Bin2} = split_binary(Bin, Length),
  Str = binary_to_list(Text),
  io:format("~4.w: Utf8                \"~s\" (~w).~n", [K, Str, Length]),
  inspectConstantPool(Bin2, K+1, ConstantPoolCount, dict:store(K, {utf8, Str}, CpDict));
inspectConstantPool(<<3, Value:32, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Integer             ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {integer, Value}, CpDict));
inspectConstantPool(<<4, Value:32, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Float               ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {float, Value}, CpDict));
inspectConstantPool(<<5, Value:64, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Long                ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount, dict:store(K, {long, Value}, CpDict));
inspectConstantPool(<<6, Value:64, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Double              ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount, dict:store(K, {double, Value}, CpDict));
inspectConstantPool(<<7, NameIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Class               NameIndex: ~w.~n", [K, NameIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {class, NameIndex}, CpDict));
inspectConstantPool(<<8, StringIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: String              StringIndex: ~w.~n", [K, StringIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {string, StringIndex}, CpDict));
inspectConstantPool(<<9, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Fieldref            ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {fieldref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<10, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: Methodref           ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {methodref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<11, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: InterfaceMethodref  ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {interfacemethodref, ClassIndex, NameAndTypeIndex}, CpDict));
inspectConstantPool(<<12, NameIndex:16, DescriptorIndex:16, Bin/binary>>, K, ConstantPoolCount, CpDict) ->
  io:format("~4.w: NameAndType         NameIndex: ~w, DescriptorIndex: ~w.~n", [K, NameIndex, DescriptorIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount, dict:store(K, {nameandtype, NameIndex, DescriptorIndex}, CpDict));
inspectConstantPool(<<Tag, _/binary>>, K, _, _) ->
  io:format("~4.w: unrecognized constant pool tag: ~w.~n", [K, Tag]).

inspectFlag(Flags, FlagBit, Name) ->
  if
    Flags band FlagBit > 0 -> io:format(" 0x~4.16.0B ~s", [FlagBit, Name]);
    true -> io:format("")
  end.

inspectClassFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SUPER"),
  inspectFlag(Flags, 16#0200, "ACC_INTERFACE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#2000, "ACC_ANNOTATION"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").

inspectInnerClassFlags(K, Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  ~4.w: InnerClassAccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [K, Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0004, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0200, "ACC_INTERFACE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#2000, "ACC_ANNOTATION"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").


inspectInterfaces(Bin, K, InterfacesCount, _) when K =:= InterfacesCount ->
  Bin;
inspectInterfaces(<<Index:16, BinTail/binary>>, K, InterfacesCount, CpDict) ->
  io:format("~4.w: Index: ~w [~s].~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectInterfaces(BinTail, K+1, InterfacesCount, CpDict).

inspectFieldFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0004, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0040, "ACC_VOLATILE"),
  inspectFlag(Flags, 16#0080, "ACC_TRANSIENT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM"),
  io:format("~n").

inspectFields(Bin, K, FieldsCount, _) when K =:= FieldsCount ->
  Bin;
inspectFields(<<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, BinTail/binary>>, K, FieldsCount, CpDict) ->
  io:format("  Field ~w of ~w:~n", [K+1, FieldsCount]),
  inspectFieldFlags(AccessFlags),
  io:format("  NameIndex: ~w [~s]~n", [NameIndex, decodeCpIndex(NameIndex, CpDict)]),
  io:format("  DescriptorIndex: ~w [~s]~n", [DescriptorIndex, decodeCpIndex(DescriptorIndex, CpDict)]),
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  Bin = inspectAttributes(BinTail, 0, AttributesCount, CpDict),
  inspectFields(Bin, K+1, FieldsCount, CpDict).


inspectMethodFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0002, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0003, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0008, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SYNCHRONIZED"),
  inspectFlag(Flags, 16#0040, "ACC_BRIDGE"),
  inspectFlag(Flags, 16#0080, "ACC_VARARGS"),
  inspectFlag(Flags, 16#0100, "ACC_NATIVE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#0800, "ACC_STRICT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  io:format("~n").

inspectMethods(Bin, K, MethodsCount, _) when K =:= MethodsCount ->
  Bin;
inspectMethods(<<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, BinTail/binary>>, K, MethodsCount, CpDict) ->
  io:format("  Method ~w of ~w:~n", [K+1, MethodsCount]),
  inspectMethodFlags(AccessFlags),
  io:format("  NameIndex: ~w [~s]~n", [NameIndex, decodeCpIndex(NameIndex, CpDict)]),
  io:format("  DescriptorIndex: ~w [~s]~n", [DescriptorIndex, decodeCpIndex(DescriptorIndex, CpDict)]),
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  Bin = inspectAttributes(BinTail, 0, AttributesCount, CpDict),
  inspectMethods(Bin, K+1, MethodsCount, CpDict).

inspectAttributes(Bin, K, AttributesCount, _) when K =:= AttributesCount ->
  Bin;
inspectAttributes(<<AttributeNameIndex:16, AttributeLength:32, BinTail/binary>>, K, AttributesCount, CpDict) ->
  io:format("  Attribute ~w of ~w:~n", [K+1, AttributesCount]),
  io:format("  AttributeNameIndex: ~w [~s]~n", [AttributeNameIndex, decodeCpIndex(AttributeNameIndex, CpDict)]),
  io:format("  AttributeLength: ~w~n", [AttributeLength]),
  {ok, {utf8, AttributeName}} = dict:find(AttributeNameIndex, CpDict),
  {AttributeBin, BinTail2} = split_binary(BinTail, AttributeLength),
  inspectAttribute(AttributeName, AttributeBin, CpDict),
  inspectAttributes(BinTail2, K+1, AttributesCount, CpDict).

inspectAttribute("Code", <<MaxStack:16,MaxLocals:16,CodeLength:32,Bin/binary>>, CpDict) ->
  io:format("  MaxStack: ~w~n", [MaxStack]),
  io:format("  MaxLocals: ~w~n", [MaxLocals]),
  io:format("  CodeLength: ~w~n", [CodeLength]),
  {CodeBin, BinTail1} = split_binary(Bin, CodeLength),
  io:format("  == code begin ==~n"),
  inspectCode(CodeBin, 0, CodeLength, CpDict),
  <<ExceptionTableLength:16, BinTail2/binary>> = BinTail1,
  io:format("  ExceptionTableLength: ~w~n", [ExceptionTableLength]),
  {ExceptionBin, BinTail2} = split_binary(BinTail2, 8*ExceptionTableLength),
  <<AttributesCount:16, BinTail3/binary>> = BinTail1,
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  inspectAttributes(BinTail3, 0, AttributesCount, CpDict);
inspectAttribute("InnerClasses", <<NumberOfClasses:16,Bin/binary>>, CpDict) ->
  io:format("  NumberOfClasses: ~w~n", [NumberOfClasses]),
  {ClassesBin, <<>>} = split_binary(Bin, NumberOfClasses*8),
  inspectInnerClass(ClassesBin, 0, NumberOfClasses, CpDict);
inspectAttribute("Signature", <<SignatureIndex:16>>, CpDict) ->
  io:format("  SignatureIndex: ~w [~s]~n", [SignatureIndex, decodeCpIndex(SignatureIndex, CpDict)]);
inspectAttribute("SourceFile", <<SourceFileIndex:16>>, CpDict) ->
  io:format("  SourceFileIndex: ~w [~s]~n", [SourceFileIndex, decodeCpIndex(SourceFileIndex, CpDict)]);
inspectAttribute(Attr, <<AttrBin/binary>>, _) ->
  io:format("  Attribute ~s binary content: ~w~n", [Attr, AttrBin]).

inspectInnerClass(_, K, NumberOfClasses, _) when K =:= NumberOfClasses ->
  io:format("");
inspectInnerClass(<<InnerClassInfoIndex:16, OuterClassInfoIndex:16, InnerNameIndex:16, InnerClassAccessFlags:16, BinTail/binary>>, K, NumberOfClasses, CpDict) ->
  io:format("  ~4.w: InnerClassInfoIndex: ~w [~s]~n", [K, InnerClassInfoIndex, decodeCpIndex(InnerClassInfoIndex, CpDict)]),
  io:format("  ~4.w: OuterClassInfoIndex: ~w [~s]~n", [K, OuterClassInfoIndex, decodeCpIndex(OuterClassInfoIndex, CpDict)]),
  io:format("  ~4.w: InnerNameIndex: ~w [~s]~n", [K, InnerNameIndex, decodeCpIndex(InnerNameIndex, CpDict)]),
  inspectInnerClassFlags(K, InnerClassAccessFlags),
  inspectInnerClass(BinTail, K+1, NumberOfClasses, CpDict).

inspectCode(_, K, CodeLength, _) when K =:= CodeLength ->
  io:format("  == code end ==~n");
inspectCode(<<18, Index:8, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ldc ~w [~s]~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+2, CodeLength, CpDict);
inspectCode(<<20, Index:16, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: ldc2_w ~w [~s]~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+3, CodeLength, CpDict);
inspectCode(<<26, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: iload_0~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<27, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: iload_1~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<28, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: iload_2~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<29, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: iload_3~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<42, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: aload_0~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<176, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: areturn~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<177, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: return~n", [K]),
  inspectCode(BinTail, K+1, CodeLength, CpDict);
inspectCode(<<178, Index:16, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: getstatic ~w [~s]~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+3, CodeLength, CpDict);
inspectCode(<<182, Index:16, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: invokevirtual ~w [~s]~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+3, CodeLength, CpDict);
inspectCode(<<183, Index:16, BinTail/binary>>, K, CodeLength, CpDict) ->
  io:format("  ~4.w: invokespecial ~w [~s]~n", [K, Index, decodeCpIndex(Index, CpDict)]),
  inspectCode(BinTail, K+3, CodeLength, CpDict);
inspectCode(<<OpCode:8, _/binary>>, K, _, _) ->
  io:format("  ~4.w: Unrecognized opcode: ~w~n", [K, OpCode]).

decodeCpIndex(Index, CpDict) ->
  {ok, Entry} = dict:find(Index, CpDict),
  decodeCpEntry(Entry, CpDict).

decodeCpEntry({utf8, Str}, _) -> Str;
decodeCpEntry({integer, Value}, _) -> "integer X";
decodeCpEntry({float, Value}, _) -> "float X";
decodeCpEntry({long, Value}, _) -> "long X";
decodeCpEntry({double, Value}, _) -> "double X";
decodeCpEntry({class, NameIndex}, CpDict) -> "class " ++ decodeCpIndex(NameIndex, CpDict);
decodeCpEntry({string, StringIndex}, CpDict) -> "string " ++ decodeCpIndex(StringIndex, CpDict);
decodeCpEntry({fieldref, ClassIndex, NameAndTypeIndex}, CpDict) -> "fieldref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({methodref, ClassIndex, NameAndTypeIndex}, CpDict) -> "methodref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({interfacemethodref, ClassIndex, NameAndTypeIndex}, CpDict) -> "interfacemethodref " ++ decodeCpIndex(ClassIndex, CpDict) ++ " " ++ decodeCpIndex(NameAndTypeIndex, CpDict);
decodeCpEntry({nameandtype, NameIndex, DescriptorIndex}, CpDict) -> "nameandtype " ++ decodeCpIndex(NameIndex, CpDict) ++ " " ++ decodeCpIndex(DescriptorIndex, CpDict).
