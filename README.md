# Eval1
- [x] `+` と `plus` の区別
    - 区別してない
- [ ] 以前のシステムで，BinOpのderiveは`rule_of_op`を加えることでまとめられる．

# EvalML1Err
- [ ] 実装する？
```
1 + true + 2 evalto error by E-PlusBoolL {  
    1 + true evalto error by E-PlusBoolR {
        true evalto true by E-True {};
    }
}

if 2 + 3 then 1 else 3 evalto error by E-IfInt {
    2 + 3 evalto 5 by E-Plus {
        2 evalto 2 by E-Int {};
        3 evalto 3 by E-Int {};
        2 plus 3 is 5 by B-Plus {}
    }
}

if 3 < 4 then 1 < true else 3 - false evalto error by E-IfTError {
    3 < 4 evalto true by E-Lt {
        3 evalto 3 by E-Int {};
        4 evalto 4 by E-Int {};
        3 less than 4 is true by B-Lt {}
    };
    1 < true evalto error by E-LtBoolR {
        true evalto true by E-Bool{};
    }
}
```

# EvalML2
- [x] 環境の実装: `variable * value list`
- [ ] `|-` の名前: `PROVE`

# EvalML3
- [x] 改行でパースが終了してしまう
- [ ] E-IfFの文字列がE-IfTになっていたので，Eval1やEval2でも間違っているはず

# NamelessML3
- [ ] judgementの名前

# EvalML4
- parser
    - [ ] 2項演算の右側にできるだけ伸ばす式が来た時の対処で，special_expがif_expになっていた
    - [ ] 過去のsystemもそう？
- [ ] Lt式の文字列表現で，右側にifが来たらかっこつけるようになってたが，いらない．過去のsystemもそう
- [ ] ConsをBinOpにすべきかどうか．教科書の定義上，2項演算になってない
- [ ] 文字列表現のかっこづけが全体的におかしい
    -　少し直した

# EvalML5
- rulebook.pdf: $res \in \mathbf{Res} ::= \mathcal E \mid F$は使わない？
