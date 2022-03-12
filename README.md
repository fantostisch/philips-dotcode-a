# Philips Dotcode A

Philips Dotcode A generator and reader.

## Usage

```shell
stack build --copy-bins --local-bin-path=.

./dotcode encode 420 > code.txt
./dotcode decode code.txt
```

## Documentation

Description on how Dotcode A works is available in the [doc](doc/) folder.

## Development

[Install The Haskell Tool Stack](https://haskellstack.org)

### Building and running

Build on every change:

```sh
make watch
```

Running:

```sh
make run
```

Running with stack traces:

```sh
make debug
```

### Testing

```sh
make test
```

### Formatting

Please format all code using [ormolu](https://github.com/tweag/ormolu) and keep lines below 90 characters.

To install ormolu, run in this directory:
```bash
stack install ormolu-0.4.0.0
```

To run ormolu on all source files:
```bash
make format
```

## License

**License**:  AGPL-3.0-or-later

```
This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Affero General Public License for more details.

You should have received a copy of the GNU Affero General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
```
