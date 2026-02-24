# COBOL→Java マイグレーション フレームワーク

COBOLソースコードからJavaへのマイグレーションを、AIエージェント（Claude Code）が段階的に実行するためのフレームワークです。

## 概要

- **目的**: COBOL→Java変換において、欠損情報を明示的に管理しながら高品質な仕様書・コードを生成する
- **対象ドメイン**: 生命保険 契約管理（バッチ処理）
- **方式**: 教師データ（過去のCOBOL/仕様書ペア）から変換ルールを学習し、新規COBOLソースに適用

## ディレクトリ構成

```
.
├── cobol-spec-framework.md    # フレームワーク定義書
├── EXECUTION_GUIDE.md         # 実行指示書（Step 0〜8）
├── DEMO_GUIDE.md              # デモ用ガイド（複数ソース対応）
├── demo/
│   ├── teacher/               # 教師データ（過去PoCペア、複数可）
│   │   ├── INSCALC.cbl        # COBOLソース：保険料計算バッチ
│   │   └── INSCALC_SPEC.md    # 対応する仕様書
│   ├── input/                 # 新規インプット
│   │   ├── programs/          # メイン/サブプログラム
│   │   │   └── CONTRENW.cbl   # COBOLソース：契約更新バッチ
│   │   └── copybooks/         # COPY句（共通コピー句）
│   └── holdout/               # 検証用正解（Step 8で使用）
│       └── CONTRENW_SPEC.md   # 正解仕様書
└── output/                    # 生成物の出力先
```

## 実行方法

1. `EXECUTION_GUIDE.md` を Claude Code に読み込ませる
2. Step 1〜8 が順次実行され、各ステップでレビューが求められる
3. 生成物は `output/` 配下に保存される

## 主な特徴

- **欠損管理**: ソースから読み取れない情報は `[欠損:D-X-NNN]` タグで明示し、推測で埋めない
- **3段階確信度**: 確信度 高/中/低 で情報を分類し、不確実な情報には `[要確認]` タグを付与
- **欠損伝播**: COBOL仕様書→Javaコード→Java仕様書へ欠損IDを一貫して伝播
