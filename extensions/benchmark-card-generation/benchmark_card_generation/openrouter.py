from __future__ import annotations

import json
import time
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from typing import Any


class OpenRouterError(RuntimeError):
    pass


@dataclass(frozen=True)
class Completion:
    content: str
    response: dict[str, Any]
    generation_id: str | None
    resolved_model: str | None
    provider: str | None
    prompt_tokens: int
    completion_tokens: int
    cost_usd: float
    latency_ms: int


def parse_json_object(text: str) -> dict[str, Any]:
    candidate = text.strip()
    if candidate.startswith("```"):
        lines = candidate.splitlines()
        if lines and lines[0].startswith("```"):
            lines = lines[1:]
        if lines and lines[-1].strip() == "```":
            lines = lines[:-1]
        candidate = "\n".join(lines).strip()
    try:
        value = json.loads(candidate)
    except json.JSONDecodeError:
        start = candidate.find("{")
        if start < 0:
            raise OpenRouterError("The model response has no JSON object")
        try:
            value, _ = json.JSONDecoder().raw_decode(candidate[start:])
        except json.JSONDecodeError as error:
            raise OpenRouterError(f"The model response has invalid JSON: {error}") from error
    if not isinstance(value, dict):
        raise OpenRouterError("The model response JSON must be an object")
    return value


class OpenRouterClient:
    def __init__(
        self,
        api_key: str,
        base_url: str = "https://openrouter.ai/api/v1",
        timeout_seconds: float = 180.0,
    ) -> None:
        if not api_key.strip():
            raise ValueError("The OpenRouter API key is empty")
        self.api_key = api_key
        self.base_url = base_url.rstrip("/")
        self.timeout_seconds = timeout_seconds

    def _request(self, method: str, path: str, body: dict[str, Any] | None = None) -> dict[str, Any]:
        data = None if body is None else json.dumps(body).encode("utf-8")
        request = urllib.request.Request(
            f"{self.base_url}{path}",
            data=data,
            method=method,
            headers={
                "Authorization": f"Bearer {self.api_key}",
                "Content-Type": "application/json",
                "HTTP-Referer": "https://github.com/hyperfiddle/electric-card-maker",
                "X-Title": "Electric Card Maker benchmark",
                "X-OpenRouter-Metadata": "enabled",
            },
        )
        try:
            with urllib.request.urlopen(request, timeout=self.timeout_seconds) as response:
                return json.loads(response.read().decode("utf-8"))
        except urllib.error.HTTPError as error:
            detail = error.read().decode("utf-8", errors="replace")[:1000]
            try:
                message = json.loads(detail).get("error", {}).get("message", detail)
            except json.JSONDecodeError:
                message = detail
            raise OpenRouterError(f"OpenRouter HTTP {error.code}: {message}") from error
        except urllib.error.URLError as error:
            raise OpenRouterError(f"OpenRouter request failed: {error.reason}") from error
        except json.JSONDecodeError as error:
            raise OpenRouterError("OpenRouter returned invalid JSON") from error

    def chat(
        self,
        model: str,
        messages: list[dict[str, str]],
        temperature: float,
        max_tokens: int,
    ) -> Completion:
        started = time.monotonic()
        response = self._request(
            "POST",
            "/chat/completions",
            {
                "model": model,
                "messages": messages,
                "temperature": temperature,
                "max_tokens": max_tokens,
            },
        )
        latency_ms = round((time.monotonic() - started) * 1000)
        choices = response.get("choices")
        if not isinstance(choices, list) or not choices:
            raise OpenRouterError("OpenRouter returned no completion choices")
        content = choices[0].get("message", {}).get("content")
        if not isinstance(content, str) or not content.strip():
            raise OpenRouterError("OpenRouter returned empty completion content")
        usage = response.get("usage") or {}
        metadata = response.get("openrouter_metadata") or {}
        return Completion(
            content=content,
            response=response,
            generation_id=response.get("id"),
            resolved_model=response.get("model"),
            provider=response.get("provider") or metadata.get("provider_name"),
            prompt_tokens=int(usage.get("prompt_tokens") or 0),
            completion_tokens=int(usage.get("completion_tokens") or 0),
            cost_usd=float(usage.get("cost") or 0.0),
            latency_ms=latency_ms,
        )

    def models(self, query: str = "", limit: int = 20) -> list[dict[str, Any]]:
        params = {"output_modalities": "text"}
        if query:
            params["q"] = query
        response = self._request("GET", f"/models?{urllib.parse.urlencode(params)}")
        models = response.get("data")
        if not isinstance(models, list):
            raise OpenRouterError("OpenRouter returned an invalid model list")
        return models[:limit]

    def model(self, model_slug: str) -> dict[str, Any]:
        slug = urllib.parse.quote(model_slug, safe="/:")
        response = self._request("GET", f"/model/{slug}")
        model = response.get("data")
        if not isinstance(model, dict):
            raise OpenRouterError("OpenRouter returned invalid model details")
        return model
