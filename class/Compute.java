class Compute {
    static long compute(int a, int b) {
        long result = 2*a-7*b;
        result += 5;
        return result;
    }
}
/*
  NameIndex: 10 [compute] DescriptorIndex: 11 [(II)J] AttributesCount: 1
  Attribute 1/1: AttributeNameIndex: 8 [Code] AttributeLength: 50
  MaxStack: 4 MaxLocals: 4 CodeLength: 18
                                 Stack              Locals
                                                    3 4
     0: iconst_2              -> 2                  3 4
     1: iload_0               -> 2 5                3 4
     2: imul                  -> 10                 3 4
     3: bipush 7 [()V]        -> 10 7               3 4
     5: iload_1               -> 10 7 4
     6: imul                  -> 10 28              3 4
     7: isub                  -> -18                3 4
     8: il2                   -> -18L               3 4
     9: lstore_2              ->                    3 4 -18L
    10: lload_2               -> -18L               3 4 -18L
    11: ldc2_w 2 [long 5]     -> -18L 5L            3 4 -18L
    14: ladd                  -> -13L
    15: lstore_2              ->                    3 4 -13L
    16: lload_2               -> -13L
    17: lreturn
 */