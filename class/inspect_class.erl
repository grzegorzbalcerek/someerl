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
  <<16#cafebabe:32,MinorVersion:16,MajorVersion:16,ConstantPoolCount:16,Bin2/binary>> = Bin,
  io:format("MinorVersion: ~w.~n", [MinorVersion]),
  io:format("MajorVersion: ~w.~n", [MajorVersion]),
  io:format("ConstantPoolCount: ~w.~n", [ConstantPoolCount]),
  Bin3 = inspectConstantPool(Bin2, 1, ConstantPoolCount),
  <<ClassFlags:16, Bin4/binary>> = Bin3,
  inspectClassFlags(ClassFlags),
  <<ThisClass:16,SuperClass:16,InterfacesCount:16,Bin5/binary>> = Bin4,
  io:format("ThisClass: ~w.~n", [ThisClass]),
  io:format("SuperClass: ~w.~n", [SuperClass]),
  io:format("InterfacesCount: ~w.~n", [InterfacesCount]),
  Bin6 = inspectInterfaces(Bin5, 0, InterfacesCount),
  <<FieldsCount:16,Bin7/binary>> = Bin6,
  io:format("FieldsCount: ~w.~n", [FieldsCount]),
  <<MethodsCount:16,Bin8/binary>> = Bin7,
  io:format("MethodsCount: ~w.~n", [MethodsCount]),
  Bin9 = inspectMethods(Bin8, 0, MethodsCount).

inspectConstantPool(Bin, K, ConstantPoolCount) when K =:= ConstantPoolCount -> Bin;
inspectConstantPool(<<1, Length:16, Bin/binary>>, K, ConstantPoolCount) ->
  {Text, Bin2} = split_binary(Bin, Length),
  io:format("~4.w: Utf8                \"~s\" (~w).~n", [K, binary_to_list(Text), Length]),
  inspectConstantPool(Bin2, K+1, ConstantPoolCount);
inspectConstantPool(<<3, Value:32, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Integer             ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<4, Value:32, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Float               ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<5, Value:64, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Long                ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount);
inspectConstantPool(<<6, Value:64, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Double              ~w.~n", [K, Value]),
  inspectConstantPool(Bin, K+2, ConstantPoolCount);
inspectConstantPool(<<7, NameIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Class               NameIndex: ~w.~n", [K, NameIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<8, StringIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: String              StringIndex: ~w.~n", [K, StringIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<9, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Fieldref            ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<10, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: Methodref           ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<11, ClassIndex:16, NameAndTypeIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: InterfaceMethodref  ClassIndex: ~w, NameAndTypeIndex: ~w.~n", [K, ClassIndex, NameAndTypeIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<12, NameIndex:16, DescriptorIndex:16, Bin/binary>>, K, ConstantPoolCount) ->
  io:format("~4.w: NameAndType         NameIndex: ~w, DescriptorIndex: ~w.~n", [K, NameIndex, DescriptorIndex]),
  inspectConstantPool(Bin, K+1, ConstantPoolCount);
inspectConstantPool(<<Tag, _/binary>>, K, _) ->
  io:format("~4.w: unrecognized constant pool tag: ~w.~n", [K, Tag]).

inspectFlag(Flags, FlagBit, Name) ->
  if
    Flags band FlagBit > 0 -> io:format("  0x~4.16.0B ~s~n", [FlagBit, Name]);
    true -> io:format("")
  end.

inspectClassFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:~n", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SUPER"),
  inspectFlag(Flags, 16#0200, "ACC_INTERFACE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC"),
  inspectFlag(Flags, 16#2000, "ACC_ANNOTATION"),
  inspectFlag(Flags, 16#4000, "ACC_ENUM").

inspectInterfaces(Bin, K, InterfacesCount) when K =:= InterfacesCount -> Bin;
inspectInterfaces(<<Index:16, Bin/binary>>, K, InterfacesCount) ->
  io:format("~4.w: Index: ~w.~n", [K, Index]),
  inspectConstantPool(Bin, K+1, InterfacesCount).

inspectMethodFlags(Flags) ->
  <<Flags4:4, Flags3:4, Flags2:4, Flags1:4>> = <<Flags:16>>,
  io:format("  AccessFlags: 0x~4.16.0B == ~4.2.0B ~4.2.0B ~4.2.0B ~4.2.0B:~n", [Flags, Flags4, Flags3, Flags2, Flags1]),
  inspectFlag(Flags, 16#0001, "ACC_PUBLIC"),
  inspectFlag(Flags, 16#0020, "ACC_PRIVATE"),
  inspectFlag(Flags, 16#0040, "ACC_PROTECTED"),
  inspectFlag(Flags, 16#0080, "ACC_STATIC"),
  inspectFlag(Flags, 16#0010, "ACC_FINAL"),
  inspectFlag(Flags, 16#0020, "ACC_SYNCHRONIZED"),
  inspectFlag(Flags, 16#0040, "ACC_BRIDGE"),
  inspectFlag(Flags, 16#0080, "ACC_VARARGS"),
  inspectFlag(Flags, 16#0100, "ACC_NATIVE"),
  inspectFlag(Flags, 16#0400, "ACC_ABSTRACT"),
  inspectFlag(Flags, 16#0800, "ACC_STRICT"),
  inspectFlag(Flags, 16#1000, "ACC_SYNTHETIC").

inspectMethods(Bin, K, MethodsCount) when K =:= MethodsCount ->
  Bin;
inspectMethods(<<AccessFlags:16, NameIndex:16, DescriptorIndex:16, AttributesCount:16, BinTail/binary>>, K, MethodsCount) ->
  io:format("  Method ~w of ~w:~n", [K+1, MethodsCount]),
  inspectMethodFlags(AccessFlags),
  io:format("  NameIndex:       ~w~n", [NameIndex]),
  io:format("  DescriptorIndex: ~w~n", [DescriptorIndex]),
  io:format("  AttributesCount: ~w~n", [AttributesCount]),
  Bin = inspectAttributes(BinTail, 0, AttributesCount),
  Bin.

inspectAttributes(Bin, K, AttributesCount) when K =:= AttributesCount ->
  Bin;
inspectAttributes(<<AttributeNameIndex:16, AttributeLength:32, BinTail/binary>>, K, AttributesCount) ->
  io:format("  Attribute ~w of ~w:~n", [K+1, AttributesCount]),
  io:format("  AttributeNameIndex: ~w~n", [AttributeNameIndex]),
  io:format("  AttributeLength:    ~w~n", [AttributeLength]).
