'use strict';

import { execSync } from 'child_process';

/**
 * アプリケーション定義
 * @type {Array<{name: string, nixShell: string, testCommand: string}>}
 */
const apps = [
  { name: 'java', nixShell: 'java', testCommand: 'gradle test' },
  { name: 'clojure', nixShell: 'clojure', testCommand: 'lein test' },
  { name: 'rust', nixShell: 'rust', testCommand: 'cargo test' },
  { name: 'scala', nixShell: 'scala', testCommand: 'sbt test' },
  { name: 'haskell', nixShell: 'haskell', testCommand: 'stack test --system-ghc --no-install-ghc' },
  { name: 'csharp', nixShell: 'dotnet', testCommand: 'dotnet test' },
  { name: 'fsharp', nixShell: 'dotnet', testCommand: 'dotnet test' },
];

/**
 * Nix が利用可能か確認する
 * @returns {boolean}
 */
function isNixAvailable() {
  try {
    execSync('nix --version', { stdio: 'ignore' });
    return true;
  } catch {
    return false;
  }
}

/**
 * 指定アプリのテストを Nix 環境で実行する
 * @param {string} nixShell - Nix devShell 名
 * @param {string} appDir - アプリケーションディレクトリ名
 * @param {string} testCommand - テストコマンド
 */
function runTest(nixShell, appDir, testCommand) {
  const cmd = `nix develop .#${nixShell} --command bash -c "cd apps/${appDir} && ${testCommand}"`;
  execSync(cmd, { stdio: 'inherit' });
}

/**
 * テストタスクを gulp に登録する
 * @param {import('gulp').Gulp} gulp - Gulp インスタンス
 */
export default function (gulp) {
  // 個別テストタスク
  for (const app of apps) {
    gulp.task(`test:${app.name}`, (done) => {
      if (!isNixAvailable()) {
        console.warn('Warning: Nix is not available. Please install Nix and try again.');
        done();
        return;
      }
      try {
        console.log(`Running ${app.name} tests...`);
        runTest(app.nixShell, app.name, app.testCommand);
        console.log(`${app.name} tests passed.`);
        done();
      } catch (error) {
        done(error);
      }
    });
  }

  // 全テスト実行タスク
  gulp.task('test', gulp.series(...apps.map((app) => `test:${app.name}`)));
}
