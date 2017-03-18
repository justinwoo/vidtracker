import {Observable} from 'rxjs';
import run from '@cycle/rxjs-run'
import {makeDOMDriver, div, h1, button, a, span} from '@cycle/dom';
import {makeHTTPDriver} from '@cycle/http';

function main(sources) {
  const files$ = sources.http.select('files')
    .flatMap(x => x)
    .map(x => x.body)
  const data$ = sources.http.select('data')
    .flatMap(x => x)
    .map(x => x.body)
  const dom = Observable.combineLatest(
    files$,
    data$,
    (files, data) =>
      div('', {
        style: {
          margin: '0 auto',
          width: '1024px'
        }
      }, [
        h1('', {}, 'Filetracker'),
        div('.files', {}, files.map(file => {
          const matches = data.filter(x => x.path === file);
          const watched = matches.length > 0;
          return div('.file', {
            style: {
              display: 'flex',
              flexDirection: 'row',
              alignItems: 'center',
              margin: '5px'
            }
          }, [
            a('.file-link', {
              style: {
                flex: 1,
                padding: '10px'
              },
              dataset: {
                file
              }
            }, file),
            button(`.file-button.pure-button${watched ? '.pure-button-primary' : ''}`, {
              style: {
                width: '200px'
              },
              dataset: {
                file,
                watched
              }
            }, watched ? 'watched' : 'not watched'),
            span('', {
              style: {
                width: '150px',
                margin: '0 10px'
              }
            }, watched ? new Date(matches[0].created).toDateString() : null)
          ])
        }))
      ])
  );
  const http = Observable.merge(
    Observable.from([{
      url: '/api/files',
      category: 'files'
    }, {
      url: '/api/watched',
      category: 'data'
    }]),
    sources.dom.select('button.file-button').events('click')
      .map(e => ({
        path: e.target.dataset.file,
        watched: e.target.dataset.watched === 'true' ? false : true
      }))
      .map(send => ({
        url: '/api/update',
        method: 'POST',
        send,
        category: 'data'
      })),
    sources.dom.select('.file-link').events('click')
      .map(e => ({
        path: e.target.dataset.file
      }))
      .map(send => ({
        url: '/api/open',
        method: 'POST',
        send
      }))
  );
  return {
    dom,
    http
  };
}

const drivers = {
  dom: makeDOMDriver('#app'),
  http: makeHTTPDriver()
}

run(main, drivers);