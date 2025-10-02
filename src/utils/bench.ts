export interface BenchmarkOptions {
  targetTotalTimeMs?: number
  maxSamples?: number
}

export interface BenchmarkResult {
  minNs: number
  samples: number
  iterPerSample: number
  timerResolutionNs: number
}

function nowNs(): bigint {
  return process.hrtime.bigint()
}

function detectTimerResolution(samples = 2000): number {
  let best = Number.MAX_SAFE_INTEGER
  let prev = nowNs()
  for (let i = 0; i < samples; i++) {
    const cur = nowNs()
    const d = Number(cur - prev)
    if (d > 0 && d < best) best = d
    prev = cur
  }
  return best
}

export function benchmarkSync(
  f: () => unknown,
  opts: BenchmarkOptions = {}
): BenchmarkResult {
  const targetTotalTimeMs = opts.targetTotalTimeMs ?? 2000
  const maxSamples = opts.maxSamples ?? 1000
  const timerResolutionNs = detectTimerResolution()
  const p0 = nowNs()
  f()
  const probeNs = Number(nowNs() - p0) || 1
  // Decide how many iterations to batch so that a single sample lasts
  // at least 10× the timer resolution.
  let iterPerSample = 1
  if (probeNs < timerResolutionNs) {
    iterPerSample = Math.ceil((timerResolutionNs * 10) / probeNs)
  }
  // Decide how many samples to collect to stay within the time budget.
  let samples = Math.ceil(
    (targetTotalTimeMs * 1e6) / (probeNs * iterPerSample)
  )
  samples = Math.max(1, Math.min(samples, maxSamples))
  let best = Number.MAX_SAFE_INTEGER
  for (let s = 0; s < samples; s++) {
    const t0 = nowNs()
    for (let i = 0; i < iterPerSample; i++) f()
    const t1 = nowNs()
    const perCall = Number(t1 - t0) / iterPerSample
    if (perCall < best) best = perCall
  }
  return { minNs: best, samples, iterPerSample, timerResolutionNs }
}
